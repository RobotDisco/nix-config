{ modulesPath, ... }:

{
  imports =
    [
      (modulesPath + "/profiles/qemu-guest.nix")
      (modulesPath + "/virtualisation/qemu-guest-agent.nix")
    ];

  # Use the GRUB 2 boot loader.
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.device = "/dev/vda"; # or "nodev" for efi only

  fileSystems."/" =
    { device = "/dev/disk/by-label/rootpart0";
      fsType = "ext4";
    };

  swapDevices =
    [ { device = "/dev/disk/by-label/swappart0"; }
    ];

  services.openssh.enable = true;
}
