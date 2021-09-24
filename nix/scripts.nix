{ pkgs }: {
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
  build-lib-script = pkgs.writeShellScriptBin "build-lib" "${pkgs.stack}/bin/stack build --fast frugel:lib --ghc-options -fdefer-type-errors";
  repl-script = pkgs.writeShellScriptBin "repl" "${pkgs.stack}/bin/stack repl --ghci-options '+RTS -N -RTS -fdefer-type-errors'";
  regen-hie-script = pkgs.writeShellScriptBin "regen-hie" "echo '2' | ${pkgs.stack}/bin/stack repl --test --ghci-options '-fwrite-ide-info -hiedir=.hie -ignore-dot-ghci'";
}
