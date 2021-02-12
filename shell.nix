let
  sources = import ./nix/sources.nix { };
in
with import sources.miso { };
let
  unstablePkgs = import sources.nixpkgs-unstable { };
  stablePkgs = import sources.nixpkgs { };
  ghcPackages = pkgs.haskell.packages.ghc865;
  gitIgnore = stablePkgs.nix-gitignore.gitignoreSourcePure;
  drv = ghcPackages.callCabal2nix "frugel" (gitIgnore [ ./.gitignore ] ./.) {
    miso = miso-jsaddle; /* Overrides miso dependency defined in package.yaml */
  };
  reload-script = stablePkgs.writeShellScriptBin "reload" ''
    ${pkgs.haskellPackages.ghcid}/bin/ghcid -c '\
        stack repl\
        --ghci-options -fno-break-on-exception\
        app/Main.hs\
        '\
        --restart=package.yaml\
        -T 'Main.main'
  '';
  floskell = unstablePkgs.haskellPackages.floskell;
  nix-pre-commit-hooks = import sources."pre-commit-hooks.nix";
  pre-commit-check = nix-pre-commit-hooks.run {
    src = ./.;
    hooks = {
      nixpkgs-fmt.enable = true;
      nix-linter.enable = true;
      hlint.enable = true;
      floskell = {
        enable = true;
        name = "floskell";
        description = "A flexible Haskell source code pretty printer.";
        entry = "${floskell}/bin/floskell";
        files = "\\.l?hs$";
      };
      build = {
        enable = true;
        name = "build";
        description = "A build of the project.";
        entry = "nix-build";
        files = "package\\.yaml$";
        pass_filenames = false;
      };
    };
  };
in
drv.env.overrideAttrs (
  old: {
    buildInputs = old.buildInputs ++ [
      reload-script
      stablePkgs.hlint
      stablePkgs.haskellPackages.apply-refact
      floskell
      stablePkgs.ghcid
      stablePkgs.stack
      pkgs.haskell.packages.ghcjs.ghc
      stablePkgs.git # has to be present for pre-commit-check shell hook
    ];
    shellHook = ''
      ${pre-commit-check.shellHook}
    '';
  }
)
