# Provide the overlays we always want to include.
# Is it better to curry or use a set where I can set defaults?
# TODO consider the readability of this.
overlays:
# This is our wrapper around darwin.lib.darwinSystem that includes a bunch of
# common configuration we want.
{
  # Our nix-darwin input
  darwin,
  # user-specific management input
  home-manager,
  # user-supplied NixOS modules or inline configuration
  darwinModules,
  # user-supplied home-mamager modules
  homeModules,
  # Include additional parameters that are handled as part of the imports
  # section of a NixOS module
  darwinSpecialArgs,
  # Include additional parameters that are handled as part of the imports
  # section of a NixOS module
  homeSpecialArgs,
  # The nix-defined CPU architecture of the host being generated
  system,
}:
darwin.lib.darwinSystem {
  inherit system;
  specialArgs = darwinSpecialArgs;
  modules = [
    # Always include the overlays we've defined in our flake, as we expect to
    # use them if we've bothered to define them
    { nixpkgs.overlays = overlays; }

    # Share the same pkgs attrset as nixos, don't create a separate one
    # for each user. If I ever define multiple users this is a potential
    # security hazard if they pull random things in via nix-env
    home-manager.darwinModules.home-manager
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
      # Supply home-manager with special arguments
      home-manager.extraSpecialArgs = homeSpecialArgs;
    }
  ] ++ darwinModules;
}
