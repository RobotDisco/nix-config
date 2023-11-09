{ pkgs, ... }:

{
  imports = [
    ./bluetooth.nix
    ../chromecast.nix
    ./power.nix
    ./removable-disks.nix
    ./touchpad.nix
    ./wireguard.nix
    ./wireless.nix
  ];

  # Enable screen locking in X
  programs.xss-lock = {
    enable = true;

    lockerCommand = "${pkgs.i3lock}/bin/i3lock -c 746542";
  };
}
