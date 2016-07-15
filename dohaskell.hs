{-# LANGUAGE ViewPatterns, QuasiQuotes, NoImplicitPrelude, OverloadedStrings #-}
-- #!/usr/bin/env nix-shell
-- #!nix-shell -i runhaskell -p 'haskellPackages.ghcWithPackages (h: with h; [ errors ] )'

import Protolude hiding (log)
import Data.String (String)
import Data.Text (unlines)
import Control.Error
import System.FilePath
import System.Process
import System.Directory
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
  filename <- toS . dropExtension <$> tryHead "no cabal file, aborting" cFiles

  slog $ filename <> ".cabal" <> " -> " <> filename <> ".nix"
  foo <- bimapExceptT show Nix.fromStorePath
    $ Nix.parseInstRealize $ programNix (toS dir) filename
  -- tmp
  scriptIO $ putStrLn foo


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
        echo "with import <nixpkgs> {}; haskellPackages.callPackage $$nixfile {}" > $$out/default.nix
        echo "(import ./default.nix).env" > $$out/shell.nix
      ''
  |]

log = putStrLn
slog = scriptIO . log
