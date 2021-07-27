{ pkgs, ... }:

{
  imports = [
    <dotfiles/setup/common>
    ./wm.nix
  ];

  environment.systemPackages = with pkgs; [
    beets
    bitwarden
    brightnessctl
    calibre
    chromium
    cifs-utils
    direnv
    discord
    dwarf-fortress-packages.dwarf-fortress-full
    file
    firefox
    fwupd
    git-crypt
    inotify-tools
    mkpasswd
    nethack
    nixops
    scrot
    seafile-client
    signal-desktop
    slack
    unzip
    yubioath-desktop
    playerctl
  ];

  security.wrappers = {
    "mount.cifs".source = "${pkgs.cifs-utils}/bin/mount.cifs";
  };

  programs.steam.enable = true;
}
