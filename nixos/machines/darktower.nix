{ config, lib, pkgs, modulesPath, ... }:

{
  # Let's lean into ZFS unless I ever need non-ZFS
  boot.supportedFilesystems = [ "zfs" ];
  # This should be parameterized, an eight-character hex string
  networking.hostId = "aa3d3177";
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
  boot.loader.grub.devices = [
    {
      devices = [ "nodev" ];
      path = "/boot/efis/efiboot0";
      efiSysMountPoint = "/boot/efis/efiboot0";
    }
    {
      devices = [ "nodev" ];
      path = "/boot/efis/efiboot1";
      efiSysMountPoint = "/boot/efis/efiboot1";
    }
  ];

  # Don't force import of root ZFS pools
  boot.zfs.forceImportRoot = false;

  time.timeZone = "America/Toronto";

  services.openssh.enable = true;

  # networking.firewall.allowedTCPPorts = [ ];
  # networking.firewall.allowedUDPPorts = [ ];

  system.stateVersion = "22.05";

  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
  ];
  
  boot.initrd.availableKernelModules = [ "xhci_pci" "ehci_pci" "ahci" "usb_storage" "usbhid" "sd_mod" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

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

  # Where containers and possibly VMs live
  fileSystems."/var/lib" = {
    device = "rootpool/nixos/var/lib";
    fsType = "zfs";
    options = [ "zfsutil" "X-mount.mkdir" ];
  };

  # Where logs live
  fileSystems."/var/log" = {
    device = "rootpool/nixos/var/log";
    fsType = "zfs";
    options = [ "zfsutil" "X-mount.mkdir" ];
  };

  fileSystems."/boot" = {
    device = "bootpool/nixos/boot";
    fsType = "zfs";
    options = [ "zfsutil" "X-mount.mkdir" ];
  };

  fileSystems."/boot/efis/efiboot0" = {
    device = "/dev/disk/by-uuid/3ECE-8042";
    fsType = "vfat";
  };

  fileSystems."/boot/efis/efiboot1" = {
    device = "/dev/disk/by-uuid/3ED1-B594";
    fsType= "vfat";
  };

  swapDevices = [
    { label = "swappart0"; }
    { label = "swappart1"; }
  ];

  networking.useDHCP = lib.mkDefault true;
  networking.interfaces.eno1 = {
    ipv4.addresses = {
      address = "192.168.10.3";
      prefixLength = 24;
    };
  };
  networking.interfaces.enp6s0f0.useDHCP = lib.mkDefault false;
  networking.interfaces.enp6s0f1.useDHCP = lib.mkDefault false;

  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
}
