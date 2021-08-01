# Packages for general software development and devops

{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    direnv
    git-crypt
    jq
    ripgrep
  ];
}
