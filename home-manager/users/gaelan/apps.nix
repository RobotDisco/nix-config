# Applications gaelan uses a lot
{ pkgs, ... }:

{
  home.packages = with pkgs; [
    # Music library organizer
    beets

    # Credential management
    bitwarden

    # Books
    calibre

    # Chat apps
    discord
    signal-desktop
    slack

    # File syncing across desktops
    seafile-client
  ];
}
