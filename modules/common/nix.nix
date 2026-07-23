{ pkgs, ... }:

{
  nixpkgs = {
    config = {
      # Enable nonfree software
      allowUnfree = true;

      # bitwarden-desktop is pinned to electron_39 upstream (EOL) in both
      # nixpkgs stable and unstable; accept the risk until upstream bumps it.
      permittedInsecurePackages = [ "electron-39.8.10" ];
    };
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

  # Install tooling that downloads pre-built non-nixpkgs derivations.
  # This avoids unnecessary compilation.
  environment.systemPackages = [ pkgs.cachix ];
}
