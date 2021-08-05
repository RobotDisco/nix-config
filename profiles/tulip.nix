{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    awscli
    google-cloud-sdk
    mattermost-desktop
  ];

  # We need strongswan support for Tulip's VPN.
  networking.networkmanager.enableStrongSwan = true;
}
