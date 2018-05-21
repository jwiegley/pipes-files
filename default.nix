{ compiler    ? "ghc822" # "ghc842" also works
, doProfiling ? false
, doBenchmark ? false
, doTracing   ? false
, doStrict    ? false
, rev         ? "95b1827682dc30ff1ccffb4f46c197289cea3e1c"
, sha256      ? "0v5s2918a04h6h1m18pzp36l5f41rhkipwqgysamsz7h0q4zwhwz"
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
    enableLibraryProfiling    = doProfiling;
    enableExecutableProfiling = doProfiling;

    inherit doBenchmark;
  });

  inherit returnShellEnv;
}
