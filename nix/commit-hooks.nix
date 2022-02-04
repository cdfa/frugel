{ pkgs, floskell }:
with pkgs;
{
  floskellHook = {
    enable = false;
    name = "Floskell";
    description = "A flexible Haskell source code pretty printer.";
    entry = "${floskell}/bin/floskell";
    files = "\\.l?hs$";
  };
  floskellConfigChangeHook = {
    enable = false;
    name = "Floskell config change";
    description = "Reformatting all Haskell files because the Floskell config has changed";
    entry = "${bash}/bin/bash -c 'shopt -s globstar; ${floskell}/bin/floskell $(${coreutils}/bin/ls {prelude,app,src,test,scout-src}/**/*.hs | ${gnused}/bin/sed \"/src\\/Optics\\/External/d\")'";
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
    entry = "${direnv}/bin/direnv exec . bash -c 'regen-hie ; weeder --require-hs-files'";
    files = "\\.l?hs$";
    pass_filenames = false;
  };
}
