{ pkgs, ... }:

{
  system.autoUpgrade = {
    enable = true;
    flake = "github:RobotDisco/nix-config";

    dates = "weekly";
  };

  # Enable firmware updates
  services.fwupd.enable = true;

  # Eventually get rid of old nix derivations
  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 30d";
  };

  # Don't allow anyone except those in the admin group to
  # perform a sudo.
  security.sudo.execWheelOnly = true;

  nixpkgs = {
    # Enable nonfree software
    config.allowUnfree = true;
  };

  nix = {
    # Enable nix flakes
    extraOptions = "experimental-features = nix-command flakes";

    # Enable binary cache downloads of standard nix packages
    settings = {
      substituters = [ "https://nix-community.cachix.org" ];
      trusted-public-keys = [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      ];
      # Automatically decide how many parallel nix jobs to run.
      max-jobs = "auto";
      # Let the root owner manage nix config
      trusted-users = [ "root" ];
    };
  };

  time.timeZone = "America/Toronto";
  i18n.defaultLocale = "en_CA.UTF-8";

  # Despite it requiring xserver, this is the only way I have found to set
  # console keyboard layouts
  console.useXkbConfig = true;

  # Enable microcode updates
  hardware.cpu.intel.updateMicrocode = true;

  system.stateVersion = "23.05";
}
