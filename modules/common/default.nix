{
  imports = [ ./nix.nix ];

  config = {
    # Enable ZSH, my preferred shell.
    programs.zsh.enable = true;
  };
}
