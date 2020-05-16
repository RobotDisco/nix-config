{ ... }:

{
  imports = [
    # Set up home-manager as a NixOS module.
    <home-manager/nixos>

    ./users-gaelan-common.nix
  ];
}
