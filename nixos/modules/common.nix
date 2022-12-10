{ config, lib, pkgs, ... }:

let cfg = config.robot-disco.common;
in {
  options.robot-disco.common.autoUpgrade = {
    enable = lib.mkOption {
      description = "Enable system autoupgrades.";
      default = true;
      type = lib.types.bool;
    };
  };
  config = lib.mkMerge [
    (lib.mkIf cfg.autoUpgrade.enable {
      system.autoUpgrade = {
        enable = true;
        flake = "github:RobotDisco/nix-config";

        dates = "daily";
      };
    })
    {
    # Enable firmware updates
    services.fwupd.enable = true;

    # Let the root owner manage nix config
    nix.settings.trusted-users = [ "root" ];

    # Eventually get rid of old nix derivations
    nix.gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 30d";
    };

    # Try out nsncd instead of ncsd, will become standard in 23.05
    services.nscd.enableNsncd = true;

    # Don't allow anyone except those in the admin group to
    # perform a sudo.
    security.sudo.execWheelOnly = true;

    nixpkgs = {
      # Enable nonfree software
      config.allowUnfree = true;
    };

    nix = {
      # Enable nix flakes
      package = pkgs.nixFlakes;
      extraOptions = ''
        experimental-features = nix-command flakes
      '';
    };

    # Enable binary cache downloads of standard nix packages
    nix.settings.substituters = [ "https://nix-community.cachix.org" ];
    nix.settings.trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];

    time.timeZone = "America/Toronto";
    i18n.defaultLocale = "en_CA.UTF-8";

    # Despite it requiring xserver, this is the only way I have found to set
    # console keyboard layouts
    console.useXkbConfig = true;

    # Enable microcode updates
    hardware.cpu.intel.updateMicrocode =
      lib.mkDefault config.hardware.enableRedistributableFirmware;

    nix.settings.max-jobs = lib.mkDefault "auto";
    system.stateVersion = "22.11";
  }];
}
