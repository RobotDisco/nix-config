{ config, pkgs, ... }:
{
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
  ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Enable sshd for bootstrapping
  services.openssh.enable = true;
  users.users.root.openssh.authorizedKeys.keyFiles = [
    /etc/ssh/authorized_keys.d/root
  ];

  networking.hostName = "nixos";
}
