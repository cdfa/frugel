{ sources, ghc }:
let
  unstablePkgs = import sources.nixpkgs-unstable {};
  stablePkgs = import sources.nixpkgs {};
  /* Don't change to fetchFromGitHub like the warning recommends, because this automatically unzips the complete archive which is huge */
  all-cabal-hashes = stablePkgs.fetchurl {
    url = "https://github.com/commercialhaskell/all-cabal-hashes/archive/ef920675c69d0a73d87ff602a3869faf0d5ae0d5.tar.gz";
    sha256 = "1dhwnj59p1ar106wxbrkjxjgi91fa49kdvfvr9d0wg0cw2z9qrp6";
  };
in
import sources.miso {
  overlays = [
    (
      self: super: {
        haskell = self.lib.recursiveUpdate super.haskell {
          packages = {
            ${ghc} = super.haskell.packages.${ghc}.override (
              old: {
                inherit all-cabal-hashes;
                overrides = unstablePkgs.lib.composeManyExtensions [
                  (old.overrides or (_: _: {}))
                  (
                    self.haskell.lib.packageSourceOverrides
                      (
                        {
                          # Due to optics
                          optics-core = "0.3.0.1";
                          optics-extra = "0.3";
                          optics-th = "0.3.0.2";
                          optics-vl = "0.2.1";
                          indexed-profunctors = "0.1";
                          th-abstraction = "0.3.1.0";
                          th-lift = "0.8.0.1";

                          # Due to it-has
                          generic-lens = "2.0.0.0";
                          generic-lens-core = "2.0.0.0";

                          # Duet to genvalidity-sydtest
                          genvalidity = "0.11.0.0";
                          validity = "0.11.0.0";
                          sydtest = "0.1.0.0";
                          safe-coloured-text = "0.0.0.0";
                          sydtest-discover = "0.0.0.0";
                          yamlparse-applicative = "0.1.0.3";
                        } // (
                          # Due to optics
                          if ghc == "ghcjs" then {} else {
                            hpack = "0.32.0";
                          }
                        )
                      )
                  )
                  (
                    # Also due to optics
                    hself: hsuper:
                      self.lib.mapAttrs
                        (_: self.haskell.lib.dontCheck)
                        (
                          self.haskell.lib.packageSourceOverrides
                            {
                              time-compat = "1.9.2.2";
                              aeson = "1.4.4.0";
                            }
                            hself
                            hsuper
                        ) // {
                        network = self.haskell.lib.appendPatch hsuper.network (
                          self.fetchurl {
                            url = "https://raw.githubusercontent.com/reflex-frp/reflex-platform/9afc9a2a1864e5a89981a4959a88c646e7441549/haskell-overlays/ghcjs-network.patch";
                            sha256 = "ffbdd93540ad2042e87543521ecd2687a306494d9c065492240e4333a4b9986c";
                          }
                        );
                      }
                  )
                ];
              }
            );
          };
        };
      }
    )
  ];
}
