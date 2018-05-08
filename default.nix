{ compiler    ? "ghc822" # "ghc842" also works
, doProfiling ? false
, doBenchmark ? false
, doTracing   ? false
, doStrict    ? false
, rev         ? "255a833e841628c0b834575664eae373e28cdc27"
, sha256      ? "022xm1pf4fpjjy69g7qz6rpqnwpjcy1l0vj49m8xmgn553cs42ch"
# , nixpkgs     ? import ((import <nixpkgs> {}).fetchFromGitHub {
#     owner = "NixOS"; repo = "nixpkgs"; inherit rev sha256; }) {
, nixpkgs     ? import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256; }) {
    config.allowUnfree = true;
    config.allowBroken = false;
  }
}:

let inherit (nixpkgs) pkgs;

  haskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = with pkgs.haskell.lib; self: super: rec {
    };
  };

in haskellPackages.developPackage {
  root = ./.;

  source-overrides = {
    hierarchy = pkgs.fetchFromGitHub {
      owner  = "jwiegley";
      repo   = "hierarchy";
      rev    = "v1.0.1";
      sha256 = "07nwqwq9c3rxn05rrq1dmr4z592klbxcwnjb47v1ya11dxvic3ma";
      # date = 2018-05-08T12:27:37-07:00;
    };
  };

  modifier = drv: pkgs.haskell.lib.overrideCabal drv (attrs: {
    enableLibraryProfiling    = doProfiling;
    enableExecutableProfiling = doProfiling;

    inherit doBenchmark;
  });
}
