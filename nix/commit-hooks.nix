{ pkgs, weeder }:
let
  inherit (import ./scripts.nix { inherit pkgs;}) regen-hie-script;
in
 with pkgs;
{
  floskellHook = {
    enable = false;
    name = "Floskell";
    description = "A flexible Haskell source code pretty printer.";
    entry = "${haskellPackages.floskell}/bin/floskell";
    files = "\\.l?hs$";
  };
  floskellConfigChangeHook = {
    enable = false;
    name = "Floskell config change";
    description = "Reformatting all Haskell files because the Floskell config has changed";
    entry = "${bash}/bin/bash -c 'shopt -s globstar; ${haskellPackages.floskell}/bin/floskell $(${coreutils}/bin/ls {app,src,test,scout-src}/**/*.hs | ${gnused}/bin/sed \"/src\\/Optics\\/External/d\")'";
    files = "floskell.json$";
    pass_filenames = false;
  };
  buildHook = {
    enable = false;
    name = "build";
    description = "A build of the project.";
    entry = "nix-build";
    files = "package\\.yaml$";
    pass_filenames = false;
  };
  weederHook = {
    enable = false;
    name = "weeder";
    description = "Check dead code";
    # Generating the .hie files with stack repl will fail when both Main.hs and Spec.hs are passed. Not much that can be done about it I think
    entry = "${bash}/bin/bash -c '${regen-hie-script}/bin/regen-hie > /dev/null 2> /dev/null ; ${weeder}/bin/weeder'";
    files = "\\.l?hs$";
    pass_filenames = false;
  };
}
