{ pkgs }:
let
  stack = "${pkgs.stack}/bin/stack";
in
{
  reload-script = pkgs.writeShellScriptBin "reload" ''
    ${pkgs.ghcid}/bin/ghcid -c '\
        ${stack} repl\
        --only-main\
        --ghci-options "-fdefer-type-errors +RTS -N -RTS"\
        '\
        --reload=www\
        --restart=package.yaml\
        -r -W
  '';
  build-lib-script = pkgs.writeShellScriptBin "build-lib" "${stack} build --fast frugel:lib --ghc-options -fdefer-type-errors";
  repl-script = pkgs.writeShellScriptBin "repl" "${stack} repl --ghci-options '+RTS -N -RTS -fdefer-type-errors'";
  regen-hie-script = pkgs.writeShellScriptBin "regen-hie" "echo \"\" | ${stack} repl --ghc-options '-fwrite-ide-info -hiedir=.hie -ignore-dot-ghci' test/Spec.hs && mv .hie/Main.hie .hie/Spec.hie && echo \"\" | ${stack} repl --ghc-options '-fwrite-ide-info -hiedir=.hie -ignore-dot-ghci'";
  format-all-script = pkgs.writeShellScriptBin "format-all" "shopt -s globstar; floskell {app,src,test,scout-src}/**/*.hs";
}
