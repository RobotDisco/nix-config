{ pkgs, ... }:

{
  system.autoUpgrade = {
    # More trouble than it's worth unless I do this as part of github actions.
    enable = false;
    flake = "github:RobotDisco/nix-config";

    dates = "weekly";
  };

  # Eventually get rid of old nix derivations
  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 30d";
  };

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
        "robot-disco.cachix.org-1:UOaR4+SF1stx8O/Z+bJD9ENNjumfabRNRvCnjwct0sg="
      ];
      # Automatically decide how many parallel nix jobs to run.
      max-jobs = "auto";
      # Let the root owner manage nix config
      trusted-users = [ "root" ];
    };
  };

  environment.systemPackages = [
    pkgs.cachix
  ];
}
