{ compiler    ? "ghc822" # "ghc842" also works
, doProfiling ? false
, doBenchmark ? false
, doTracing   ? false
, doStrict    ? false
, rev         ? "9d0b6b9dfc92a2704e2111aa836f5bdbf8c9ba42"
, sha256      ? "096r7ylnwz4nshrfkh127dg8nhrcvgpr69l4xrdgy3kbq049r3nb"
, nixpkgs     ? import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256; }) {
    config.allowUnfree = true;
    config.allowBroken = false;
  }
, provideDrv  ? !nixpkgs.pkgs.lib.inNixShell
}:

let inherit (nixpkgs) pkgs;

  haskellPackages' = pkgs.haskell.packages.${compiler};

  haskellPackages = pkgs.lib.fix (this: haskellPackages'.override {
    overrides = with pkgs.haskell.lib; self: super: {
      developPackage =
        { root
        , source-overrides ? {}
        , overrides ? self: super: {}
        , modifier ? drv: drv
        , provideDrv ? !pkgs.lib.inNixShell }:
        let drv =
          (this.extend
             (pkgs.lib.composeExtensions
                (self.packageSourceOverrides source-overrides)
                overrides))
          .callCabal2nix (builtins.baseNameOf root) root {};
        in if provideDrv then modifier drv else (modifier drv).env;
    };
  });

in haskellPackages.developPackage {
  root = ./.;

  source-overrides = {
    exceptions = "0.10.0";
    hierarchy = pkgs.fetchFromGitHub {
      owner  = "jwiegley";
      repo   = "hierarchy";
      rev    = "v1.0.2";
      sha256 = "1a7q8f78ylyqmm0xyc9ma7nid4fn4sbw47sjc2d7815acn6mh29r";
      # date = 2018-05-10T23:29:32-07:00;
    };
  };

  modifier = drv: pkgs.haskell.lib.overrideCabal drv (attrs: {
    enableLibraryProfiling    = doProfiling;
    enableExecutableProfiling = doProfiling;

    inherit doBenchmark;
  });

  inherit provideDrv;
}
