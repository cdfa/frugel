let
  sources = import ./nix/sources.nix {};
in
  with import sources.miso {};
  let
    ghcjsPackages = pkgs.haskell.packages.ghcjs;
    ghcPackages = pkgs.haskell.packages.ghc865;
    reload-script = pkgs.writeShellScriptBin "reload" ''
      ${pkgs.haskellPackages.ghcid}/bin/ghcid -c '\
          ghci\
          -isrc -iapp\
          -fno-break-on-exception\
          app/Main.hs\
          '\
          --restart=package.yaml\
          -T 'Main.main'
    '';
    drv = ghcPackages.callCabal2nix "frugel" ./. {
      miso = miso-jsaddle; /* Overrides miso dependency defined in package.yaml */
    };
  in
    drv.env.overrideAttrs (
      old: {
        buildInputs = old.buildInputs ++ [ reload-script pkgs.haskellPackages.ghcid ];
      }
    )
