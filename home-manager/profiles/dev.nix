# Packages for general software development and devops
# This should be home-manager stuff honestly

{ pkgs, ... }:

{
  home.packages = with pkgs; [
    # Nix-based development
    any-nix-shell

    # Storing secrets in git
    git-crypt

    # Searching through text
    jq
    ripgrep
  ];

  programs.direnv = {
    enable = true;
    enableZshIntegration = true;
    nix-direnv = {
      enable = true;
      enableFlakes = true;
    };
  };
}
