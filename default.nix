{ compiler    ? "ghc822"
, doBenchmark ? false
, doTracing   ? false
, doStrict    ? false
, rev         ? "49bdae006e66e70ad3245a463edc01b5749250d3"
, sha256      ? "1ijsifmap47nfzg0spny94lmj66y3x3x8i6vs471bnjamka3dx8p"
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
