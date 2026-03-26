# Self-contained NixOS system builder with overlay composition
#
# This function handles all the complex overlay composition, home-manager
# integration, and module setup automatically. You just provide the basic
# system configuration.
{
  emacs-overlay,
  home-manager,
  nixpkgs,
  nixpkgs-unstable,
  ...
}:

{
  # Core system configuration
  system,
  modules,
  # Optional parameters with sensible defaults
  specialArgs ? { },
  homeModules ? [ ],
  homeSpecialArgs ? { },
}:

let
  # Compose all overlays with overrides for broken packages
  overlays = [
    # Include the community emacs overlay for latest packages
    emacs-overlay.overlays.default

    # Add our custom packages
    (final: _prev: {
      sunsama = final.callPackage ../packages/sunsama.nix { };
    })
  ];

  # Instantiate nixpkgs for this system
  # We know we'll need to allow unfree packages.
  pkgs-unstable = import nixpkgs-unstable {
    inherit system;
    config.allowUnfree = true;
  };
in
nixpkgs.lib.nixosSystem {
  inherit system;
  modules = [
    # Always include the overlays we've defined in our flake, as we expect to
    # use them if we've bothered to define them
    { nixpkgs.overlays = overlays; }

    # Share the same pkgs attrset as nixos, don't create a separate one
    # for each user. If I ever define multiple users this is a potential
    # security hazard if they pull random things in via nix-env
    home-manager.nixosModules.home-manager
    {
      # Use the system pkgs/nixpkgs options instead of generating a separate one
      # for home-manager
      home-manager.useGlobalPkgs = true;
      # Allow users to include their own packages via
      # NixOS' user.users.<name>.packages instead of forcing the use of the
      # system-level environment.systemPackages.
      home-manager.useUserPackages = true;
    }
    {
      # Right now I make my home-manager modules available to all users.
      # I may change this in the future but I will keep it hardwired for now
      home-manager.sharedModules = [ ../home-manager/modules ] ++ homeModules;
    }
    {
      # Supply home-manager with special arguments. Always include flake inputs.
      home-manager.extraSpecialArgs = {
        inherit pkgs-unstable;
      }
      // homeSpecialArgs;
    }
  ]
  ++ modules;
  specialArgs = {
    inherit pkgs-unstable;
  }
  // specialArgs;
}
