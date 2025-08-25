{ pkgs, ... }:

{
  home.packages = [
    # Browser
    pkgs.brave
    pkgs.vivaldi

    # Chat / Messaging
    pkgs.discord
    pkgs.signal-desktop
    pkgs.slack
    pkgs.whatsapp-for-linux
  ];
}
