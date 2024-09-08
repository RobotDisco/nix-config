{
  imports = [
    ../common

    ../hardware/ssd.nix

    ./desktop
    ./home-devices

    ./audio.nix
    ./bluetooth.nix
    ./power-management.nix
    ./removable-disks.nix
    ./steam.nix
    ./touchpad.nix
    ./wireless.nix
    ./yubikey.nix
  ];
}
