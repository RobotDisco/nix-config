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
    beets
    direnv
    google-cloud-sdk
  ];

  time.timeZone = "America/Toronto";
}
