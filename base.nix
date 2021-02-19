{ sources, ghc }:
let
  stablePkgs = import sources.nixpkgs { };

  miso = import sources.miso { };
  misoPkgs = miso.pkgs;

  gitIgnore = stablePkgs.nix-gitignore.gitignoreSourcePure;

  megaparsec9 = misoPkgs.haskell.packages.${ghc}.callCabal2nix "megaparsec" sources.megaparsec { };
  relude07 = misoPkgs.haskell.packages.${ghc}.callCabal2nix "relude" sources.relude { };
in
misoPkgs.haskell.packages.${ghc}.callCabal2nix "frugel" (gitIgnore [ ./.gitignore ] ./.) {
  megaparsec = megaparsec9;
  relude = miso.pkgs.haskell.lib.dontCheck relude07;
}
