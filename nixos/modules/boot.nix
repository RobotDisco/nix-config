{
  ## Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # To make my life easier I've come up with a partition naming scheme that's
  # common to all of my NixOS devices.
 
  fileSystems."/" = {
    device = "/dev/disk/by-label/rootpart0";
    fsType = "ext4";
    options = [ "noatime" ];
  };

  fileSystems."/home" = {
    device = "/dev/disk/by-label/homepart0";
    fsType = "ext4";
    options = [ "noatime" ];
  };

  fileSystems."/nix" = {
    device = "/dev/disk/by-label/nixpart0";
    fsType = "ext4";
    options = [ "noatime" ];
  };

  fileSystems."/var" = {
    device = "/dev/disk/by-label/varpart0";
    fsType = "ext4";
    options = [ "relatime" ];
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-label/EFIBOOT0";
    fsType = "vfat";
  };

  swapDevices = [{ device = "/dev/disk/by-label/swappart0"; }];
}
