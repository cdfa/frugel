{ sources, ghc }:
let
  stablePkgs = import sources.nixpkgs { };
  all-cabal-hashes = stablePkgs.fetchurl {
    url = "https://github.com/commercialhaskell/all-cabal-hashes/archive/b7c1cf48a61e0b6cfe4ed5ed1c890d1bb817fb3b.tar.gz";
    sha256 = "1b7pr7ap6jz80s7258w676z1zaq2ldh1lgca7z7v313y7fk0y8lv";
  };
in
self: super: {
  haskell = self.lib.recursiveUpdate super.haskell {
    packages = {
      ${ghc} = super.haskell.packages.${ghc}.override (
        _: {
          inherit all-cabal-hashes;
        }
      );
    };
  };
}
