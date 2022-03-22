{ config, lib, modulesPath, ... }:

{
  imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];

  boot.initrd.availableKernelModules = [ "xhci_pci" "thunderbolt" "nvme" "usb_storage" "usbhid" "sd_mod" ];
  boot.initrd.kernelModules = [ "dm-snapshot" ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.kernelParams = [ "mem_sleep_default=deep" ];
  boot.extraModulePackages = [ ];

  fileSystems."/" =
    { device = "/dev/disk/by-label/rootpart0";
      fsType = "ext4";
      options = [ "noatime" ];
    };

  fileSystems."/home" =
    { device = "/dev/disk/by-label/homepart0";
      fsType = "ext4";
      options = [ "noatime" ];
    };

  fileSystems."/nix" =
    { device = "/dev/disk/by-label/nixpart0";
      fsType = "ext4";
      options = [ "noatime" ];
    };

  fileSystems."/var" =
    { device = "/dev/disk/by-label/nixpart0";
      fsType = "ext4";
      options = [ "relatime" ];
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-label/EFIBOOT0";
      fsType = "vfat";
    };

  swapDevices = [ { device = "/dev/disk/by-label/swappart0"; } ];

  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
  # high-resolution display
  hardware.video.hidpi.enable = true;
}
