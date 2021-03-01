{ sources, ghc }:
let
  unstablePkgs = import sources.nixpkgs-unstable { };
in
self: super: {
  haskell = self.lib.recursiveUpdate super.haskell {
    packages = {
      ${ghc} = super.haskell.packages.${ghc}.override (
        old: {
          overrides = unstablePkgs.lib.composeManyExtensions [
            (old.overrides or (_: _: { }))
            (
              self.haskell.lib.packageSourceOverrides
                (
                  {
                    optics = "0.3";
                    optics-core = "0.3.0.1";
                    optics-extra = "0.3";
                    optics-th = "0.3.0.2";
                    indexed-profunctors = "0.1";
                    th-abstraction = "0.3.1.0";
                    th-lift = "0.8.0.1";
                  } // (
                    if ghc == "ghcjs" then { } else {
                      hpack = "0.32.0";
                    }
                  )
                )
            )
            (
              hself: hsuper:
                self.lib.mapAttrs
                  (_: self.haskell.lib.dontCheck)
                  (
                    self.haskell.lib.packageSourceOverrides
                      {
                        # indexed-traversable = "0.1.1";
                        time-compat = "1.9.2.2";
                        aeson = "1.4.4.0";

                        #   vector = "0.12.2.0";
                        #   th-abstraction = "0.4.2.0";
                        #   generic-deriving = "1.14";
                        #   bifunctors = "5.5.10";
                        #   base-orphans = "0.8.4";
                        #   comonad = "5.0.8";
                        #   tagged = "0.8.6.1";
                        #   invariant = "0.5.4";
                        #   lens = "4.19.2";
                        #   these = "1.1.1.1";
                        #   data-fix = "0.3.1";
                        #   primitive = "0.7.1.0";
                        #   strict = "0.4.0.1";
                        #   unordered-containers = "0.2.13.0";
                        #   quickcheck-classes-base = "0.6.1.0";
                        #   assoc = "1.0.2";
                        #   foldl = "1.4.10";
                        #   hedgehog = "1.0.4";
                        #   tasty-hedgehog = "1.0.1.0";
                        #   jsaddle = "0.9.7.1";
                        #   servant = "0.18.2";
                        #   http-api-data = "0.4.2";
                      }
                      hself
                      hsuper
                  ) // {
                  network = self.haskell.lib.appendPatch hsuper.network (
                    # Todo: change to fetchpatch
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
