{ sources ? import ./nix/sources.nix { }
, ghc ? "ghc8107"
}:
let
  haskellNix = import sources.haskellNix {};
  pkgs = import haskellNix.sources.nixpkgs-2105 haskellNix.nixpkgsArgs;
  hsPkgs = import ./base.nix { inherit sources ghc; };
  frugel-js = hsPkgs.projectCross.ghcjs.hsPkgs.frugel.components.exes.frugel-exe;
in
pkgs.stdenv.mkDerivation {
    name = "frugel-web";
    src = pkgs.haskell-nix.haskellLib.cleanGit {
      name = "frugel";
      src = ./.;
      subDir = "www";
    };
    buildInputs = [ pkgs.closurecompiler ];
    installPhase = ''
      mkdir -p $out
      find . \( -name '*.html' -o -name '*.css' \) -exec cp {} $out \;
      FRUGEL_WEB=${frugel-js}/bin/frugel-exe.jsexe
      closure-compiler \
        $FRUGEL_WEB/all.js --externs $FRUGEL_WEB/all.js.externs \
        -O ADVANCED --jscomp_off=checkVars -W QUIET \
        --js_output_file $out/all.min.js
    '';
  }
