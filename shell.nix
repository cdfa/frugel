{ sources ? import ./nix/sources.nix { }
, ghc ? "ghc865"
}:
let
  stablePkgs = import sources.nixpkgs { };
  miso = import ./nix/miso.nix { inherit sources ghc; };
  pkgs = miso.pkgs;
  base = (import ./base.nix { inherit sources pkgs ghc; }).override {
    miso = miso.miso-jsaddle; /* Overrides dependencies defined in package.yaml */
  };

  reload-script = stablePkgs.writeShellScriptBin "reload" ''
    ${stablePkgs.ghcid}/bin/ghcid -c '\
        ${stablePkgs.stack}/bin/stack repl\
        --ghci-options "-fdefer-type-errors -Wno-all +RTS -N -RTS"\
        '\
        --restart=package.yaml\
        -T 'Main.main'
  '';

  floskell = stablePkgs.haskellPackages.floskell;
  nix-pre-commit-hooks = import sources."pre-commit-hooks.nix";
  pre-commit-check = nix-pre-commit-hooks.run {
    src = ./.;
    hooks = with import ./nix/commit-hooks.nix { inherit stablePkgs floskell; }; {
      nixpkgs-fmt.enable = true;
      nix-linter.enable = true;
      hlint.enable = true;
      floskell = floskellHook // {
        enable = true;
      };
      build = buildHook // {
        enable = true;
      };
      floskellConfigChange = floskellConfigChangeHook // {
        enable = true;
      };
    };
  };
in
base.env.overrideAttrs (
  old: {
    buildInputs = old.buildInputs ++ [
      reload-script
      stablePkgs.hlint
      stablePkgs.haskellPackages.apply-refact
      floskell
      stablePkgs.ghcid
      stablePkgs.stack
      stablePkgs.cabal-install
      pkgs.haskell.packages.ghcjs.ghc
      stablePkgs.git # has to be present for pre-commit-check shell hook
    ];
    shellHook = ''
      ${pre-commit-check.shellHook}
    '';
  }
)
