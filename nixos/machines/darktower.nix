{ config, lib, pkgs, modulesPath, ... }:

{
  networking.hostName = "darktower";
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
  boot.loader.grub.mirroredBoots = [
    {
      devices = [ "nodev" ];
      path = "/boot/efis/EFIBOOT0";
      efiSysMountPoint = "/boot/efis/EFIBOOT0";
    }
    {
      devices = [ "nodev" ];
      path = "/boot/efis/EFIBOOT1";
      efiSysMountPoint = "/boot/efis/EFIBOOT1";
    }
  ];

  # Don't force import of root ZFS pools
  boot.zfs.forceImportRoot = false;
  boot.zfs.forceImportAll = false;

  users.users.root.initialHashedPassword = "$6$rounds=2500000$J3xTIRDh1NBHNJS/$gsXMT1hWNEHseQBGdYKADCNvPOl5xAP/Bb5v0fy8zwsieIS6jPfe9.HvoEKu3Wf8DkVY8/ZHOHRIPxK9unuso1";
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
    device = "bootpool/nixos/root";
    fsType = "zfs";
    options = [ "zfsutil" "X-mount.mkdir" ];
  };

  fileSystems."/boot/efis/EFIBOOT0" = {
    device = "/dev/disk/by-label/EFIBOOT0";
    fsType = "vfat";
  };

  fileSystems."/boot/efis/EFIBOOT1" = {
    device = "/dev/disk/by-label/EFIBOOT1";
    fsType= "vfat";
  };

  swapDevices = [
    { label = "swappart0"; }
    { label = "swappart1"; }
  ];

  networking.useDHCP = lib.mkDefault true;
  networking.interfaces.eno1 = {
    ipv4.addresses = [{
      address = "192.168.10.3";
      prefixLength = 24;
    }];
  };
  networking.interfaces.enp6s0f0.useDHCP = lib.mkDefault false;
  networking.interfaces.enp6s0f1.useDHCP = lib.mkDefault false;

  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;

  services.fstrim.enable = true;
  services.zfs.trim.enable = true;

  # Scrub ZFS pools every bimonthly
  services.zfs.autoScrub = {
    interval = "*-*-01,15 03:00";
    enable = true;
  };

  nixpkgs.config.packageOverrides = pkgs: {
    zfs = pkgs.zfs.override {
      enableMail = true;
    };
  };
  services.zfs.zed = {
    enableMail = true;
    settings = {
      ZED_EMAIL_ADDR = [ "gdcosta@gmail.com" ];
    };
  };

  users.users.gaelan = {
    shell = pkgs.zsh;
    isNormalUser = true;
    home = "/home/gaelan";
    description = "Gaelan D'costa";
    extraGroups = [ "wheel" ];
    # passwordFile = "/run/secrets/users_gaelan_password";
    # temp password just to get me by
    initialHashedPassword = "$6$rounds=2500000$PFL/U0wWeXAEL2j$Xf5r7J6quFZYeHtUlNQwjJIPmlZDwS7bRg5u8yWYq2NGF8WLdyiMbK.n1ymvmR3gbT7nDZ4Pdp/MvbOsyOi0E/";
  };

  nix.trustedUsers = [ "gaelan "];

  services.sshguard.enable = true;
}
