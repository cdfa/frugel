{ sources, ghc }:
import sources.miso {
  overlays = [
    (import ./all-cabal-hashes-overlay.nix { inherit sources ghc; })
  ];
}
