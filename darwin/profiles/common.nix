# Configuration for every MacOS system I make
{ pkgs, ... }:

{
  # Set regional settings I'm most likely in
  time.timeZone = "America/Toronto";

  # Run nix in multi-user mode
  services.nix-daemon.enable = true;

  nix = {
    # Enable nix flakes
    package = pkgs.nixUnstable;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';

    # Enable binary cache downloads of standard nix packages
    binaryCaches = [
      "https://nix-community.cachix.org"
    ];
    binaryCachePublicKeys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
  };

  nixpkgs = {
    # Enable nonfree software
    config.allowUnfree = true;
  };

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;
}
