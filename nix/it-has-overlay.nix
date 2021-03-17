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
                {
                  it-has = "0.2.0.0";
                  generic-lens = "2.0.0.0";
                  generic-lens-core = "2.0.0.0";
                }
            )
          ];
        }
      );
    };
  };
}
