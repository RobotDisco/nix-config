{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    awscli
    google-cloud-sdk
    mattermost-desktop
  ];
}
