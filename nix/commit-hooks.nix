{ floskell }:
{
  floskellHook = {
    enable = false;
    name = "floskell";
    description = "A flexible Haskell source code pretty printer.";
    entry = "${floskell}/bin/floskell";
    files = "\\.l?hs$";
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
