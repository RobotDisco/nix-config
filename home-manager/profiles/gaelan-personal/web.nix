{ pkgs, ... }:

{
  home.packages = [
    # Browser
    pkgs.brave

    # Chat / Messaging
    pkgs.discord
    pkgs.signal-desktop
    pkgs.slack
    pkgs.whatsapp-for-linux
  ];
}
