# This should get moved out of this repo and into a tulip specific flake
{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    awscli
    chefdk
    google-cloud-sdk
    kubectl
    mattermost-desktop
    python39Packages.virtualenv
  ];

  # We need strongswan support for Tulip's VPN.
  networking.networkmanager.enableStrongSwan = true;
}
