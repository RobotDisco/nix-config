# Packages for general software development and devops
# This should be home-manager stuff honestly

{ pkgs, ... }:

{
  home.packages = with pkgs; [
    # Nix-based development
    direnv
    any-nix-shell

    # Storing secrets in git
    git-crypt

    # Searching through text
    jq
    ripgrep
  ];
}
