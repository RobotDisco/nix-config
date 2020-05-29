{ ... }:

{
  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
    }))
  ];

  # Share as much of system packages w/ home-manager
  home-manager.useUserPackages = true;
  home-manager.useGlobalPkgs = true;

  # I'm fine installing non-free software.
  nixpkgs.config.allowUnfree = true;
}
