{ sources, pkgs, ghc }:
let
  stablePkgs = import sources.nixpkgs { };

  gitIgnore = stablePkgs.nix-gitignore.gitignoreSourcePure;

  megaparsec9 = pkgs.haskell.packages.${ghc}.callHackage "megaparsec" "9.0.1" { };
  relude07 = pkgs.haskell.packages.${ghc}.callHackage "relude" "0.7.0.0" { };
  prettyprinter170 = pkgs.haskell.packages.${ghc}.callHackage "prettyprinter" "1.7.0" { };
in
pkgs.haskell.packages.${ghc}.callCabal2nix "frugel" (gitIgnore [ ./.gitignore ] ./.) {
  megaparsec = megaparsec9;
  relude = pkgs.haskell.lib.dontCheck relude07;
  prettyprinter = prettyprinter170;
}
