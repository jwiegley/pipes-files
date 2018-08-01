{ compiler    ? "ghc822"
, doBenchmark ? false
, doTracing   ? false
, doStrict    ? false
, rev         ? "d1ae60cbad7a49874310de91cd17708b042400c8"
, sha256      ? "0a1w4702jlycg2ab87m7n8frjjngf0cis40lyxm3vdwn7p4fxikz"
, pkgs        ? import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256; }) {
    config.allowUnfree = true;
    config.allowBroken = false;
  }
, returnShellEnv ? pkgs.lib.inNixShell
, mkDerivation ? null
}:

let haskellPackages = pkgs.haskell.packages.${compiler};

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
    inherit doBenchmark;
  });

  inherit returnShellEnv;
}
