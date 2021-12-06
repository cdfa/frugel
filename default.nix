{ sources ? import ./nix/sources.nix { }
, ghc ? "ghc8107"
}:
let
  haskellNix = import sources.haskellNix { };
  pkgs = import haskellNix.sources.nixpkgs-unstable haskellNix.nixpkgsArgs;
  hsPkgs = import ./base.nix { inherit pkgs ghc; };
  frugel-cross = { platform }: hsPkgs.projectCross.${platform}.hsPkgs.frugel.components.exes.frugel-exe;
in
{
  frugel-web = pkgs.stdenv.mkDerivation {
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
      FRUGEL_WEB=${frugel-cross { platform = "ghcjs"; }}/bin/frugel-exe.jsexe
      closure-compiler \
        $FRUGEL_WEB/all.js --externs $FRUGEL_WEB/all.js.externs \
        -O ADVANCED --jscomp_off=checkVars -W QUIET \
        --js_output_file $out/all.min.js
    '';
  };
  # frugel-aarch64-darwin = frugel-cross { platform = "aarch64-darwin"; }; Not working due to https://github.com/NixOS/nixpkgs/issues/49526
  # frugel-gnu64 = frugel-cross { platform = "gnu64"; }; Not working due to https://github.com/NixOS/nixpkgs/issues/56493 ?
  # frugel-mingwW64 = frugel-cross { platform = "mingwW64"; }; Not working due to https://github.com/NixOS/nixpkgs/issues/36200
}
