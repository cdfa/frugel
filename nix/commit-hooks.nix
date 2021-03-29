{ stablePkgs, floskell }:
{
  floskellHook = {
    enable = false;
    name = "Floskell";
    description = "A flexible Haskell source code pretty printer.";
    entry = "${floskell}/bin/floskell";
    files = "^(?!^src\\/Optics\\/External).*\\.l?hs$";
  };
  floskellConfigChangeHook = {
    enable = false;
    name = "Floskell config change";
    description = "Reformatting all Haskell files because the Floskell config has changed";
    entry = "${stablePkgs.bash}/bin/bash -c 'shopt -s globstar; ${floskell}/bin/floskell $(${stablePkgs.coreutils}/bin/ls {app,src,test}/**/*.hs | ${stablePkgs.gnused}/bin/sed \"/src\\/Optics\\/External/d\")'";
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
}
