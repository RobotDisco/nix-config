{ myLib, ... }:

{
  imports = myLib.scanPaths ./.;

  config = {
    # Enable ZSH, my preferred shell.
    programs.zsh.enable = true;
  };
}
