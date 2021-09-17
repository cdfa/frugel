{ sources, ghc }:
let
  haskellNix = import sources.haskellNix {};
  pkgs = import haskellNix.sources.nixpkgs-unstable haskellNix.nixpkgsArgs;
  fetchpatch = pkgs.fetchpatch;
  callHpack = name: src: pkgs.runCommand "hpack2cabal-${name}" {} ''
    mkdir -p $out
    cp -r ${src}/. $out/
    rm "$out/frugel.cabal"
    ${pkgs.hpack}/bin/hpack '${src}' - > "$out/frugel.cabal"
  '';
in
pkgs.haskell-nix.cabalProject {
  src = callHpack "frugel" (
    pkgs.haskell-nix.haskellLib.cleanGit {
      name = "frugel";
      src = ./.;
    }
  );
  compiler-nix-name = ghc;
  modules = [
    {
      packages.miso.patches = [
        (
          (
            fetchpatch {
              name = "prevent-firefox-spinning-xhr.patch";
              url = "https://github.com/cdfa/miso/commit/4e1a6ee7c18a63a501ffaf08227011181e427b1a.patch";
              sha256 = "185mxvmdg3x7vpdw800w0lqj2via2z8snb9wys015bv1y1yi5q2i";
            }
          )
        )
      ];
    }
  ];
}
