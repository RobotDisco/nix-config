{ config, lib, ... }:

let cfg = config.robot-disco.boot;

in {
  options.robot-disco.boot.enableFDE = lib.mkEnableOption {
      description = "Enable yubikey-based FDE as per https://nixos.wiki/wiki/Yubikey_based_Full_Disk_Encryption_(FDE)_on_NixOS";
  };

  config = lib.mkMerge [
    (lib.mkIf cfg.enableFDE {
      # Minimal list of modules to use the EFI system partition and Yubikey
      boot.initrd.kernelModules = [
        "vfat"
        "nls_cp437"
        "nls_iso8859-1"
        "usbhid"
      ];

      # Enable support for Yubikey PBA
      boot.init.luks.yubikeySupport = true;

      # Configuration to use luks w/ yubikey
      boot.initrd.luks.device = {
        nixoscrypt = {
          device = "/dev/nvmen0p2";
          # Set to false if you need things like networking to happen first
          preLVM = true;

          yubikey = {
            slot = 2; # Long press on my yubikey.
            twoFactor = true; # Set false if you don't use password
            storage = {
              device = "/dev/disk/by-label/EFIBOOT0";
            };
          };
        };
      };
    })
    {
      # To make my life easier I've come up with a partition naming scheme
      # that's common to all of my NixOS devices.

      fileSystems."/" = {
        device = "/dev/rootvg/rootpart0";
        fsType = "btrfs";
        options = [ "subvol=@root" "compress=zstd" "noatime" ];
      };
      fileSystems."/home" = {
        device = "/dev/rootvg/rootpart0";
        fsType = "btrfs";
        options = [ "subvol=@home" "compress=zstd" "noatime" ];
      };
      fileSystems."/nix" = {
        device = "/dev/rootvg/rootpart0";
        fsType = "btrfs";
        options = [ "subvol=@nix" "compress=zstd" "noatime" ];
      };
      fileSystems."/boot" = {
        device = "/dev/disk/by-label/EFIBOOT0";
        fsType = "vfat";
        options = [ "noatime" ];
      };

      swapDevices = [{ device = "/dev/rootvg/swappart0"; }];
    }
  ];
}
