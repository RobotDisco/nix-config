{ pkgs, ... }:

{
  home.packages = [
    # Browser
    pkgs.vivaldi

    # Chat / Messaging
    pkgs.discord
    pkgs.signal-desktop
    pkgs.slack
    pkgs.wasistlos
  ];
}
