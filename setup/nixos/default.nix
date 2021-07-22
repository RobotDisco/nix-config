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
    chromium
    cifs-utils
    dunst
    feh
    file
    firefox
    fwupd
    dwarf-fortress-packages.dwarf-fortress-full
    inotify-tools
    mkpasswd
    nethack
    scrot
    seafile-client
    signal-desktop
    unzip
    yubioath-desktop
    playerctl
  ];

  security.wrappers = {
    "mount.cifs".source = "${pkgs.cifs-utils}/bin/mount.cifs";
  };

  programs.steam.enable = true;
}
