{ config, pkgs, ... }:

# This is a special host-specific .nix file since
# darktower is a baremetal server running Intel AMT
# and ZFS, which require certain specific tweaks.

{
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
  ];

  # modeset screws up Intel AMT Remote Control for some reason.
  boot.kernelParams = [ "nomodeset" ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # handle case where too many hardlinks in nix store for ZFS.
  boot.loader.grub.copyKernels = true;
  
  # Enable ZFS for booting
  boot.supportedFilesystems = [ "zfs" ];

  # Enable sshd for bootstrapping
  services.openssh.enable = true;
  users.users.root.openssh.authorizedKeys.keyFiles = [
    /etc/ssh/authorized_keys.d/root
  ];

  networking.hostName = "darktower";
  # ZFS requires a networking hostID
  networking.hostId = "526b897e";
}
