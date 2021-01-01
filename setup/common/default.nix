{ pkgs, ... }:

{
  imports = [
    ./emacs.nix
    ./fonts.nix
    ./gnupg.nix
    ./packages.nix
    ./shell.nix
  ];

  environment.systemPackages = with pkgs; [
    awscli
    direnv
    google-cloud-sdk
    jq
    ripgrep
  ];

  time.timeZone = "America/Toronto";
}
