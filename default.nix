let
  sources = import ./nix/sources.nix {};
in
with import sources.miso {};
pkgs.haskellPackages.developPackage {
  root = ./.;
  modifier = drv:
    pkgs.haskell.lib.addBuildTools drv (
      with pkgs.haskellPackages;
      [
        cabal-install
      ]
    );
}
