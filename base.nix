{ sources, pkgs, ghc }:
let
  stablePkgs = import sources.nixpkgs { };

  gitIgnore = stablePkgs.nix-gitignore.gitignoreSourcePure;

  packageSourceOverrides = pkgs.lib.mapAttrs (
    name: version:
      pkgs.haskell.packages.${ghc}.callHackage name version { }
  );
  overrides = packageSourceOverrides {
    megaparsec = "9.0.1";
    relude = "0.7.0.0";
    optics = "0.3";
    it-has = "0.2.0.0";
    genvalidity-sydtest = "0.0.0.0";
    genvalidity-text = "0.7.0.2";
    genvalidity-containers = "0.9.0.0";
  };
in
pkgs.haskell.packages.${ghc}.callCabal2nix "frugel" (gitIgnore [ ./.gitignore ] ./.)
  (
    overrides // {
      relude = pkgs.haskell.lib.dontCheck overrides.relude;
    }
  )
