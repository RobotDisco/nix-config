{ ... }:

let
  btrfs-options = [ "compress=zstd" "noatime" ];
in
{
  imports = [
    ./hardware-configuration.nix

    ../../profiles/android.nix
    ../../profiles/audio.nix
    ../../profiles/firmware.nix
    ../../profiles/hidpi.nix
    ../../profiles/home-devices
    ../../profiles/keyboard.nix
    ../../profiles/laptop
    ../../profiles/nix.nix
    ../../profiles/regional.nix
    ../../profiles/security.nix
    ../../profiles/ssd.nix
    ../../profiles/steam.nix
    ../../profiles/sway.nix
    ../../profiles/yubikey.nix
    ../../../users/gaelan
  ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "arrakis";

  # Filesystem overrides
  fileSystems."/".options = btrfs-options;
  fileSystems."/nix".options = btrfs-options;
  fileSystems."/home".options = btrfs-options;
  fileSystems."/boot".options = [ "noatime" ];

  # Simple password-based Full Disk Encryption
  boot.initrd.luks.devices = {
    nixoscrypt = {
      device = "/dev/nvme0n1p2";

      # I'd rather have TRIM support than perfect security
      allowDiscards = true;
      # increase performance on SSDs
      bypassWorkqueues = true;

      # Set to false if you need things like networking to happen first
      preLVM = true;
    };
  };

  # Enable dynamic configuration of primary NIC
  networking.interfaces.wlp1s0.useDHCP = true;

  # Support thunderbolt manager software
  services.hardware.bolt.enable = true;

  # Framework firmware is in the lvfs-testing repo
  services.fwupd.extraRemotes = [
    "lvfs-testing"
  ];
}
