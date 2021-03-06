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
    prettyprinter = "1.7.0";
  };
in
pkgs.haskell.packages.${ghc}.callCabal2nix "frugel" (gitIgnore [ ./.gitignore ] ./.)
  (
    overrides // {
      relude = pkgs.haskell.lib.dontCheck overrides.relude;
    }
  )