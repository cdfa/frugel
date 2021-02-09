let
  sources = import ./nix/sources.nix {};
in
with import sources.miso {};
pkgs.haskell.packages.ghcjs.callCabal2nix "frugel" ./. { }
