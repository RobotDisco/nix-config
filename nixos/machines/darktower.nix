{ config, lib, pkgs, modulesPath, ... }:

{
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

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
  };

  fileSystems."/home" = {
    device = "rootpool/nixos/home";
    fsType = "zfs";
  };

  fileSystems."/var/lib" = {
    device = "rootpool/nixos/var/lib";
    fsType = "zfs";
  };

  fileSystems."/var/log" = {
    device = "rootpool/nixos/var/log";
    fsType = "zfs";
  };

  fileSystems."/boot" = {
    device = "bootpool/nixos/root";
    fsType = "zfs";
  };

  fileSystems."/boot/efis/ata-WDC_WDS500G2B0A-00SM50_19107F801948-part1" = {
    device = "/dev/disk/by-uuid/3ECE-8042";
    fsType = "vfat";
  };

  fileSystems."/boot/efis/ata-WDC_WDS500G2B0a-00SM50_19136D800887-part1" = {
    device = "/dev/disk/by-uuid/3ED1-B594";
    fsType= "vfat";
  };

  fileSystems."/boot/efi" = {
    device = "/boot/efis/ata-WDC_WDS500G2B0A-00SM50_19107F801948-part1";
    fsType = "none";
    options = [ "bind" ];
  };

  swapDevices = [ ];

  networking.useDHCP = lib.mkDefault true;
  networking.interfaces.eno1.useDHCP = lib.mkDefault true;
  networking.interfaces.enp6s0f0.useDHCP = lib.mkDefault true;
  networking.interfaces.enp6s0f1.useDHCP = lib.mkDefault true;

  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
}
