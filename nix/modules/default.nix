{ ... }:

{
  imports = [
    # Set up home-manager as a NixOS module.
    <home-manager/nixos>

    ./common.nix
    ./users-gaelan-common.nix
    ./linux-laptop.nix
  ];
}
