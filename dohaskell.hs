{-# LANGUAGE ViewPatterns, QuasiQuotes, NoImplicitPrelude, OverloadedStrings #-}
-- #!/usr/bin/env nix-shell
-- #!nix-shell -i runhaskell -p 'haskellPackages.ghcWithPackages (h: with h; [ errors ] )'

import Protolude hiding (log)
import Data.String (String)
import Data.Text (unlines)
import Control.Error
-- import Control.Monad.Trans.Except
-- import Data.List (stripPrefix)
import System.FilePath
import System.Process
import System.Directory
-- import System.IO (withFile, IOMode(WriteMode))
import Control.Error.Script

import NeatInterpolation (text)
import qualified Foreign.Nix.Shellout as Nix

-- TODO: autogenerate nix files on change of cabal file
-- TODO: use default.nix if it already exists, don’t generate shim in that case
-- TODO: boot into <command> afterwards

main = runScript $ do
  dir <- scriptIO getCurrentDirectory
  cFiles <- scriptIO $ filter ((==".cabal").takeExtension) <$> listDirectory dir
  when (length cFiles > 1) $
    throwE "more than one cabal files, aborting"
  cFile <- tryHead "no cabal file, aborting" cFiles

  let filename = toS $ dropExtension cFile
  slog $ filename<>".cabal" <> " -> " <> filename<>".nix"
  -- exceptT _fo _ba-- (throwE.show) (scriptIO . putStrLn . Nix.fromStorePath)
  foo <- bimapExceptT show Nix.fromStorePath
    $ Nix.parseInstRealize $ programNix (toS dir) filename
  scriptIO $ putStrLn foo

  -- let defaultNix = dir </> "default.nix"
  -- pure . writeIfNotExists defaultNix $ [text|
  --   with import <nixpkgs> {};
  --   haskellPackages.callPackage ${defaultNix};
  -- |]

  where
    writeIfNotExists :: FilePath -> Text -> IO ()
    writeIfNotExists f s = do
          e <- not <$> doesFileExist f
          when e $ do
            log $ "writing " <> toS f
            writeFile f s

log :: Text -> IO ()
log = putStrLn
slog = scriptIO . log



programNix folder filename = (\a -> trace a a)
  -- escaping twice with $${} to not splice in haskell values
  [text|
    with import <nixpkgs> {};
    let
      c2n = lib.getBin haskellPackages.cabal2nix;
    in
      runCommand "${filename}-nixfiles" {} ''
        mkdir $$out
        nixfile=$$out/${filename}.nix
        $${c2n}/bin/cabal2nix ${folder} > $$nixfile
        echo "with import <nixpkgs> {}; haskellPackages.callPackage $$nixfile {}" > $$out/default.nix
        echo "(import ./default.nix).env" > $$out/shell.nix
      ''
  |]
