# Packages for general software development and devops
# This should be home-manager stuff honestly

{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    direnv
    git-crypt
    jq
    ripgrep
  ];

  virtualisation.docker.enable = true;
}
