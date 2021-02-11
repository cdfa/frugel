let
  sources = import ./nix/sources.nix { };
in
with import sources.miso { };
let
  gitIgnore = pkgs.nix-gitignore.gitignoreSourcePure;
  base-noprelude-ghcjs = pkgs.haskell.packages.ghcjs.callCabal2nix "base-noprelude-ghcjs" sources.base-noprelude { };
in
pkgs.haskell.packages.ghcjs.callCabal2nix "frugel" (gitIgnore [ ./.gitignore ] ./.) {
  base-noprelude = base-noprelude-ghcjs;
}
