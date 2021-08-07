# These shouldn't be system packages. These are just applications I use a lot
{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    # Music
    beets

    # Credential Management
    bitwarden

    # Books
    calibre

    # Browsers
    chromium
    firefox

    # Chat apps
    discord
    signal-desktop
    slack

    # File syncing across desktops
    seafile-client

    # Games
    dwarf-fortress-packages.dwarf-fortress-full
    nethack
  ];

  programs.steam.enable = true;
}
