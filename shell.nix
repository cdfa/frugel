let
  sources = import ./nix/sources.nix {};
in
  with import sources.miso {};
  let
    ghcPackages = pkgs.haskell.packages.ghc865;
    reload-script = pkgs.writeShellScriptBin "reload" ''
      ${pkgs.haskellPackages.ghcid}/bin/ghcid -c '\
          stack repl\
          --ghci-options -fno-break-on-exception\
          app/Main.hs\
          '\
          --restart=package.yaml\
          -T 'Main.main'
    '';
    gitIgnore = pkgs.nix-gitignore.gitignoreSourcePure;
    drv = ghcPackages.callCabal2nix "frugel" (gitIgnore [ ./.gitignore ] ./.) {
      miso = miso-jsaddle; /* Overrides miso dependency defined in package.yaml */
    };
  in
    drv.env.overrideAttrs (
      old: {
        buildInputs =
          let
            unstablePkgs = import sources.nixpkgs-unstable {};
            stablePkgs = import sources.nixpkgs {};
          in
            old.buildInputs ++ [
              reload-script
              stablePkgs.hlint
              stablePkgs.haskellPackages.apply-refact
              unstablePkgs.haskellPackages.floskell
              stablePkgs.ghcid
              stablePkgs.stack
              pkgs.haskell.packages.ghcjs.ghc
            ];
      }
    )
