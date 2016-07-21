{-# LANGUAGE ViewPatterns, QuasiQuotes, NoImplicitPrelude, OverloadedStrings, TupleSections #-}

import Protolude hiding (log)
import Data.String (String)
import Data.Text (unlines)
import Control.Error
import System.FilePath
import System.Process
import System.Directory
import System.Posix.Files (createSymbolicLink)
import System.IO.Error (isAlreadyExistsError)
import Control.Error.Script

import NeatInterpolation (text)
import qualified Foreign.Nix.Shellout as Nix
import Foreign.Nix.Shellout (StorePath, Realized, NixAction)

-- TODO: autogenerate nix files on change of cabal file
-- TODO: boot into <command> afterwards
-- TODO: should create symlinks? Maybe better to do it the other way around?

data StoreFile = StoreFile { path :: FilePath
                           , drv  :: StorePath Realized }

main :: IO ()
main = realMain =<< getCurrentDirectory

realMain :: FilePath -> IO ()
realMain currentDir = runScript $ do
  dirFiles <- scriptIO $ listDirectory currentDir
    >>= mapM (\f -> let p = currentDir </> f
                    in (p,) <$> isSymbolicLink p)

  let filterFiles f = map fst $ filter f dirFiles
      cabFiles = filterFiles $ \(f,_) -> ".cabal" == takeExtension f
      nixFiles = filterFiles $ \(f,s) -> not s && ".nix" == takeExtension f

  -- check for a cabal file
  when (length cabFiles > 1) $
    throwE "more than one cabal files, aborting"
  filename <- toS . dropExtension . takeFileName
    <$> tryHead "no cabal file, aborting" cabFiles

  -- actual work
  storeFiles <- forM nixFiles
    $ \f -> StoreFile f <$> (nixToScript identity $ Nix.addToStore f)
  outDir <- nixToScript Nix.fromStorePath
    $ Nix.parseInstRealize $ programNix storeFiles (toS currentDir) filename

  scriptIO $ putStrLn outDir
  scriptIO $ outDir `linkEachTo` currentDir

  where
    nixToScript :: Show a => (b -> c) -> NixAction a b -> NixAction String c
    nixToScript = bimapExceptT show
    linkEachTo fromdir todir =
      listDirectory fromdir >>= mapM_ (symlink todir . (fromdir</>))
    symlink todir from =
      let tofile = todir </> takeFileName from
      in catchJust (guard.isAlreadyExistsError)
                   (createSymbolicLink from tofile)
                   (const $ whenM (isSymbolicLink tofile)
                              $ removeFile tofile *> symlink todir from)

-- This is an abomination.
programNix :: [StoreFile] -> Text -> Text -> Text
programNix infiles folder filename =
  -- escaping twice with $${} to not splice in Haskell values
  [text|
    with import <nixpkgs> {};
    let
      c2n = lib.getBin haskellPackages.cabal2nix;
    in
      runCommand "${filename}-nixfiles" {} ''
        mkdir $$out

        ${outfiles}

        nixfile=$$out/${filename}.nix
        $${c2n}/bin/cabal2nix ${folder} > $$nixfile

        if [[ ! -a $$out/default.nix ]]; then
          echo "with import <nixpkgs> {}; haskellPackages.callPackage $$nixfile {}" > $$out/default.nix
        fi

        echo "(import ./default.nix).env" > $$out/shell.nix
      ''
  |]
  where
    outfiles :: Text
    outfiles = unlines $ flip map infiles
      $ \(StoreFile path drv) ->
          "cat " <> (toS $ Nix.fromStorePath drv)
                 <> " > " <> "$out/" <> (toS $ takeFileName path)
