# Applications gaelan uses a lot
{ pkgs, ... }:

{
  home.packages =
    # home-manager seemas basically useless on Darwin for packages
    if pkgs.stdenv.isDarwin
    then []
    else with pkgs; [
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
