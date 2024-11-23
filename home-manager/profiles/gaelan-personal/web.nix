{ pkgs, ... }:

{
  home.packages = [
    # Browser
    pkgs.brave

    # Chat / Messaging
    pkgs.webcord
    pkgs.signal-desktop
    pkgs.slack
    pkgs.whatsapp-for-linux
  ];
}
