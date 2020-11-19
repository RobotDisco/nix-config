# Do not modify this file!  It was generated by ‘nixos-generate-config’
# and may be overwritten by future invocations.  Please make changes
# to /etc/nixos/configuration.nix instead.
{ config, lib, pkgs, ... }:

{
  imports =
    [ <nixpkgs/nixos/modules/profiles/qemu-guest.nix>
    ];

  boot.initrd.availableKernelModules = [ "ata_piix" "uhci_hcd" "virtio_pci" "sr_mod" "virtio_blk" ];
  boot.initrd.kernelModules = [ "dm-snapshot" ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  # handle case where too many hardlinks in nix store for ZFS.
  boot.loader.grub.copyKernels = true;

  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.device = "/dev/vda";

  networking.useDHCP = false;
  networking.interfaces.ens3.useDHCP = true;

  fileSystems."/" =
    { device = "/dev/disk/by-label/rootpart";
      fsType = "ext4";
    };

  fileSystems."/home" =
    { device = "/dev/disk/by-label/homepart";
      fsType = "ext4";
    };

  fileSystems."/nix" =
    { device = "/dev/disk/by-label/nixpart";
      fsType = "ext4";
    };

  fileSystems."/var" =
    { device = "/dev/disk/by-label/varpart";
      fsType = "ext4";
    };

  swapDevices =
    [ { device = "/dev/disk/by-label/swappart"; }
    ];

  nix.maxJobs = lib.mkDefault 1;
}
