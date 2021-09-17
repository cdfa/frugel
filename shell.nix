{ sources ? import ./nix/sources.nix { }
, ghc ? "ghc8107"
}:
let
  haskellNix = import sources.haskellNix { };
  pkgs = import haskellNix.sources.nixpkgs-2105 haskellNix.nixpkgsArgs;

  hsPkgs = import ./base.nix { inherit sources ghc; };

  reload-script = pkgs.writeShellScriptBin "reload" ''
    ${pkgs.ghcid}/bin/ghcid -c '\
        ${pkgs.stack}/bin/stack repl\
        --only-main\
        --ghci-options "-fdefer-type-errors +RTS -N -RTS"\
        '\
        --reload=www\
        --restart=package.yaml\
        -r -W
  '';

  weeder = (
    pkgs.haskell-nix.hackage-package
      {
        compiler-nix-name = ghc;
        name = "weeder";
        version = "latest";
      }
  ).components.exes.weeder;

  nix-pre-commit-hooks = import sources."pre-commit-hooks.nix";
  pre-commit-check = nix-pre-commit-hooks.run {
    src = ./.;
    hooks = with import ./nix/commit-hooks.nix { inherit pkgs weeder; }; {
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
      weeder = weederHook // {
        enable = true;
      };
    };
  };
in
hsPkgs.shellFor {
  tools = {
    cabal = "3.4.0.0";
    hlint = "latest"; # Selects the latest version in the hackage.nix snapshot
    haskell-language-server = "latest";
    stan = "latest";
  };
  buildInputs = with pkgs; [
    reload-script
    haskellPackages.floskell
    ghcid
    stack
    git # required by pre-commit-check shell hook
    weeder
    dhall-lsp-server
    (
      pkgs.haskell-nix.hackage-package
        {
          compiler-nix-name = ghc;
          name = "apply-refact";
          version = "latest";
        }
    ).components.exes.refactor
  ];
  withHoogle = false;
  exactDeps = false;
  shellHook = ''
    ${pre-commit-check.shellHook}
  '';
}
