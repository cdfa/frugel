{ sources ? import ./nix/sources.nix { }
, ghc ? "ghc8107"
}:
let
  haskellNix = import sources.haskellNix { };
  pkgs = import haskellNix.sources.nixpkgs-unstable haskellNix.nixpkgsArgs;

  hsPkgs = import ./base.nix { inherit pkgs ghc; };

  floskell = (
    pkgs.haskell-nix.hackage-package
      {
        compiler-nix-name = ghc;
        name = "floskell";
        version = "latest";
        modules = [
          {
            packages.floskell.ghcOptions = [ "-threaded" "-with-rtsopts=-N" ];
          }
        ];
      }
  ).components.exes.floskell;

  nix-pre-commit-hooks = import "${sources."pre-commit-hooks.nix"}/nix" { nixpkgs = haskellNix.sources.nixpkgs-unstable; };
  pre-commit-check = nix-pre-commit-hooks.run {
    src = ./.;
    hooks = with import ./nix/commit-hooks.nix { inherit pkgs floskell; }; {
      nixpkgs-fmt.enable = true;
      nix-linter.enable = true;
      hlint.enable = true;
      prettier = {
        enable = true;
        files = "www/.*\\.css|.*\\.md$";
      };
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
    hlint = "latest";
    stan = "latest";
  };
  buildInputs = with pkgs; [
    floskell
    ghcid
    stack
    git # required by pre-commit-check shell hook
    dhall-lsp-server
    (
      pkgs.haskell-nix.hackage-package
        {
          compiler-nix-name = ghc;
          name = "weeder";
          version = "2.2.0";
        }
    ).components.exes.weeder
    (
      pkgs.haskell-nix.hackage-package
        {
          compiler-nix-name = ghc;
          name = "apply-refact";
          version = "latest";
        }
    ).components.exes.refactor
    (
      pkgs.haskell-nix.hackage-package
        {
          compiler-nix-name = ghc;
          name = "haskell-language-server";
          version = "latest";
          configureArgs = "-frename --allow-newer=hls-rename-plugin:ghcide";
        }
    ).components.exes.haskell-language-server
  ] ++ builtins.attrValues (import ./nix/scripts.nix { inherit pkgs floskell; });
  withHoogle = false;
  exactDeps = false;
  shellHook = ''
    ${pre-commit-check.shellHook}
  '';
  STACK_YAML = "nix-stack.yaml";
}
