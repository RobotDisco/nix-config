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

  # Assume we need monitor hotplug support
  services.autorandr = { enable = true; };

  # Enable screen locking in X
  programs.xss-lock = {
    enable = true;

    lockerCommand = "${pkgs.i3lock}/bin/i3lock -c 746542";
  };
}
