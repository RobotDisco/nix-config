# Self-contained standalone home-manager configuration builder with overlay
# composition — mirrors nixosSystem.nix/darwinSystem.nix. Used for the
# `homeConfigurations` flake output, which lets `just build-home` /
# `just switch-home` iterate on home-manager config without a full
# NixOS/nix-darwin system rebuild.
{
  emacs-overlay,
  home-manager,
  nixpkgs,
  nixpkgs-unstable,
  tarot-emacs,
  ...
}:

{
  # Core configuration
  system,
  modules,
  hostName,
  # Optional parameters with sensible defaults
  extraSpecialArgs ? { },
}:

let
  # Compose all overlays with overrides for broken packages
  overlays = [
    # Include the community emacs overlay for latest packages
    emacs-overlay.overlays.default

    (import ../packages/overlay.nix)
  ];

  # Instantiate nixpkgs for this system
  # We know we'll need to allow unfree packages.
  pkgs = import nixpkgs {
    inherit system overlays;
    config.allowUnfree = true;
  };

  pkgs-unstable = import nixpkgs-unstable {
    inherit system;
    config.allowUnfree = true;
  };

  # Tarot's emacs package for whichever system we're building for.
  emacsTarot = tarot-emacs.packages.${system}.default;
in
home-manager.lib.homeManagerConfiguration {
  inherit pkgs;
  modules = [ ../home-manager/modules ] ++ modules;
  extraSpecialArgs = {
    inherit pkgs-unstable hostName emacsTarot;
  }
  // extraSpecialArgs;
}
