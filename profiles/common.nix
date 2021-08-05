# Every nixos install I own should have these configured
{ pkgs, ... }:

{
  # These are handy misc. utilities to have around
  environment.systemPackages = with pkgs; [
    file
    mkpasswd
    unzip
  ];

  # Enable upgrade management for firmware
  services.fwupd.enable = true;

  # Enable nonFree firmware
  hardware.enableRedistributableFirmware = true;

  # All my hardware is Intel, so
  hardware.cpu.intel.updateMicrocode = true;

  # Enable locking of machines (for when I step away)
  programs.slock.enable = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.05"; # Did you read the comment?
}
