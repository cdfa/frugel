{ sources ? import ./nix/sources.nix { }
, ghc ? "ghcjs"
}:
let
  miso = import sources.miso { };
  base-noprelude-ghcjs =
    miso.pkgs.haskell.packages.${ghc}.callCabal2nix "base-noprelude-ghcjs" sources.base-noprelude { };
  base = import ./base.nix { inherit sources ghc; };
in
base.override {
  base-noprelude = base-noprelude-ghcjs;
}
