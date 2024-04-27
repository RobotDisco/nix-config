{ pkgs, ... }:

{
  nixpkgs = {
    # Enable nonfree software
    config.allowUnfree = true;
  };

  nix = {
    # Enable nix flakes
    extraOptions = "experimental-features = nix-command flakes";

    # Enable automatic garbage collection.
    gc.automatic = true;
    # Enable automatic nix store file deduplication.
    optimise.automatic = true;

    # Enable binary cache downloads of standard nix packages
    settings = {
      substituters = [ "https://nix-community.cachix.org" ];
      trusted-public-keys = [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "robot-disco.cachix.org-1:UOaR4+SF1stx8O/Z+bJD9ENNjumfabRNRvCnjwct0sg="
      ];
      # Automatically decide how many parallel nix jobs to run.
      max-jobs = "auto";
    };
  };

  environment.systemPackages = [ pkgs.cachix ];

  # Sometimes it's easier / more feasible to use legacy nix commands that rely
  # on the old <nixpkgs> channel. Since nix flakes don't manage traditional
  # nixchannels, let's set it here to pin <nixpkgs> to our current nixpkgs
  # input.
  #
  # But this is already set when you set up a nixos system via flakes
  # so we don't need to set up anything on NixOS
  #
  # environment.variables.NIX_PATH = [ "nixpkgs=${pkgs.path}" ];
}
