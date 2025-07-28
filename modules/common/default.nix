{ myLib, ... }:

{
  imports = [
    ../../secrets/agenix-rekey.nix
  ]
  ++ (myLib.scanPaths ./.);

  config = {
    # Enable ZSH, my preferred shell.
    programs.zsh.enable = true;
  };
}
