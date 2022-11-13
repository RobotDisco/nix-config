{ config, lib, ... }:

let cfg = config.robot-disco.boot;

in {
  options.robot-disco.boot = {
    hostId = lib.mkOption {
      description = "eight-digit hex number, primarily used by ZFS.";
      example = "12ab34cd";
    };
  };

  config = {
    # Let's lean into ZFS unless I ever need non-ZFS
    boot.supportedFilesystems = [ "zfs" ];
    # This should be parameterized, an eight-character hex string
    networking.hostId = cfg.hostId;
    boot.kernelPackages = config.boot.zfs.package.latestCompatibleLinuxPackages;

    # Don't allow system to change EFI variables
    boot.loader.efi.canTouchEfiVariables = false;
    # Copy kernel files into /boot so /nix/store isn't needed
    boot.loader.generationsDir.copyKernels = true;
    # TL;DR Don't depend on NVRAM state
    boot.loader.grub.efiInstallAsRemovable = true;
    # OpenZFS recommends grub; sure, why not
    boot.loader.grub.enable = true;
    boot.loader.grub.version = 2;
    # Copy kernels to /boot
    boot.loader.grub.copyKernels = true;
    boot.loader.grub.efiSupport = true;
    boot.loader.grub.zfsSupport = true;
    # Since we mirror our system drives, mirror boot partitions.
    boot.loader.grub.mirroredBoots = [{
      devices = [ "nodev" ];
      path = "/boot/efis/EFIBOOT0";
      efiSysMountPoint = "/boot/efis/EFIBOOT0";
    }];

    # Don't force import of root ZFS pools
    boot.zfs.forceImportRoot = false;
    boot.zfs.forceImportAll = false;

    # To make my life easier I've come up with a partition naming scheme that's
    # common to all of my NixOS devices.

    fileSystems."/" = {
      device = "rootpool/nixos/root";
      fsType = "zfs";
      options = [ "zfsutil" "X-mount.mkdir" ];
    };

    fileSystems."/home" = {
      device = "rootpool/nixos/home";
      fsType = "zfs";
      options = [ "zfsutil" "X-mount.mkdir" ];
    };

    # Where logs live
    fileSystems."/var/log" = {
      device = "rootpool/nixos/var/log";
      fsType = "zfs";
      options = [ "zfsutil" "X-mount.mkdir" ];
    };

    fileSystems."/var/lib" = {
      device = "rootpool/nixos/var/lib";
      fsType = "zfs";
      options = [ "zfsutil" "X-mount.mkdir" ];
    };

    fileSystems."/boot" = {
      device = "bootpool/nixos/root";
      fsType = "zfs";
      options = [ "zfsutil" "X-mount.mkdir" ];
    };

    fileSystems."/boot/efis/EFIBOOT0" = {
      device = "/dev/disk/by-label/EFIBOOT0";
      fsType = "vfat";
    };

    swapDevices = [{ device = "/dev/disk/by-label/swappart0"; }];
  };
}
