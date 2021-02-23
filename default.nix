{ sources ? import ./nix/sources.nix { }
, ghc ? "ghcjs"
}:
let
  miso = import ./nix/miso.nix { inherit sources ghc; };
  pkgs = miso.pkgs;
  base-noprelude-ghcjs =
    pkgs.haskell.packages.${ghc}.callCabal2nix "base-noprelude-ghcjs" sources.base-noprelude { };
  base = import ./base.nix { inherit sources pkgs ghc; };
in
base.override {
  base-noprelude = base-noprelude-ghcjs;
}
