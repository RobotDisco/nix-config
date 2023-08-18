{
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
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

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
