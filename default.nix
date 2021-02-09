let
  sources = import ./nix/sources.nix {};
in
  with import sources.miso {};
  let
    gitIgnore = pkgs.nix-gitignore.gitignoreSourcePure;
  in
    pkgs.haskell.packages.ghcjs.callCabal2nix "frugel" (gitIgnore [ ./.gitignore ] ./.) {}
