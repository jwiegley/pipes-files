{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, attoparsec, base, bytestring, directory
      , doctest, exceptions, filepath, free, hierarchy, hspec
      , hspec-expectations, mmorph, monad-control, mtl, pipes, pipes-safe
      , posix-paths, process, regex-posix, semigroups, stdenv, text, time
      , transformers, transformers-base, transformers-compat, unix
      , unix-compat
      }:
      mkDerivation {
        pname = "pipes-files";
        version = "0.1.0";
        src = ./.;
        libraryHaskellDepends = [
          attoparsec base bytestring directory exceptions filepath free
          hierarchy mmorph monad-control mtl pipes pipes-safe posix-paths
          regex-posix semigroups text time transformers transformers-base
          transformers-compat unix unix-compat
        ];
        testHaskellDepends = [
          base bytestring directory doctest filepath hierarchy hspec
          hspec-expectations mtl pipes pipes-safe process semigroups text
          transformers unix
        ];
        homepage = "https://github.com/jwiegley/pipes-files";
        description = "Fast traversal of directory trees using pipes";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                      then pkgs.haskellPackages
                      else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
