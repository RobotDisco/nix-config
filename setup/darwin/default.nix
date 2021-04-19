{config, pkgs, ...}:

{
  imports = [
    ../common
    ./emacs.nix
  ];

  environment.systemPackages = with pkgs; [
    Bitwarden
    # Calibre # Uses an APFS dmg which isn't supported by undmg
    # Deezer
    Discord
    Firefox
    Kobo
    Remarkable
    SeafileClient
    Signal
    Slack
    Steam
    Tidal
  ];

  # Spotlight (and Alfred) don't index symlinks, and the ~/Applications and/or
  # ~/Applications/Nix Apps are symlinks to /nix dirs, so that just don't work.
  #
  # Our hack for now, until nix-darwin figures out a real answer to this, is
  # to change the applications hook to create aliases into /Applications.

  # Sources and inspirations:
  # https://github.com/LnL7/nix-darwin/blob/b26b1e89c1f62f06abfb315505ebae9f1d5c3282/modules/system/applications.nix
  # https://github.com/LnL7/nix-darwin/issues/139#issuecomment-666771621
  
  system.activationScripts.applications.text = pkgs.lib.mkForce (''
    # Set up applications.
    echo "setting up ~/Applications..." >&2
    
    if [ ! -e ~/Applications -o -L ~/Applications ]; then
      ln -sfn ${config.system.build.applications}/Applications ~/Applications
    elif [ ! -e ~/Applications/Nix\ Apps -o -L ~/Applications/Nix\ Apps ]; then
      ln -sfn ${config.system.build.applications}/Applications ~/Applications/Nix\ Apps
    else
      echo "warning: ~/Applications and ~/Applications/Nix Apps are directories, skipping App linking..." >&2
    fi

    find ${config.system.build.applications}/Applications -maxdepth 1 -type l | while read f; do
      echo "Linking $f"
      src=$(/usr/bin/stat -f%Y $f)
      appname="$(basename $src)"
      osascript -e "do shell script \"/bin/rm /Applications/$appname\"";
      osascript -e "tell app \"Finder\" to make alias file at POSIX file \"/Applications/\" to POSIX file \"$src\" with properties {name: \"$appname\"}";
      done
    ''
  );
}
