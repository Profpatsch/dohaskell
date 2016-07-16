{-# LANGUAGE ViewPatterns, QuasiQuotes, NoImplicitPrelude, OverloadedStrings #-}

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

-- TODO: autogenerate nix files on change of cabal file
-- TODO: boot into <command> afterwards

main = runScript $ do
  dir <- scriptIO getCurrentDirectory
  cFiles <- scriptIO $ filter ((==".cabal").takeExtension) <$> listDirectory dir
  when (length cFiles > 1) $
    throwE "more than one cabal files, aborting"
  filename <- toS . dropExtension
    <$> tryHead "no cabal file, aborting" cFiles

  outDir <- bimapExceptT show Nix.fromStorePath
    $ Nix.parseInstRealize $ programNix (toS dir) filename

  scriptIO $ outDir `linkEachTo` dir

  where
    linkEachTo fromdir todir =
      listDirectory fromdir >>= mapM_ (symlink todir . (fromdir</>))
    symlink todir from =
      let tofile = todir </> takeFileName from
      in catchJust (guard.isAlreadyExistsError)
                   (createSymbolicLink from tofile)
                   (const $ whenM (isSymbolicLink tofile)
                              $ removeFile tofile *> symlink todir from)


programNix :: Text -> Text -> Text
programNix folder filename =
  -- escaping twice with $${} to not splice in Haskell values
  [text|
    with import <nixpkgs> {};
    let
      c2n = lib.getBin haskellPackages.cabal2nix;
    in
      runCommand "${filename}-nixfiles" {} ''
        mkdir $$out
        nixfile=$$out/${filename}.nix
        $${c2n}/bin/cabal2nix ${folder} > $$nixfile
        if [[ ! -a ${folder}/default.nix ]]; then
          echo "with import <nixpkgs> {}; haskellPackages.callPackage $$nixfile {}" > $$out/default.nix
        fi
        echo "(import ./default.nix).env" > $$out/shell.nix
      ''
  |]

