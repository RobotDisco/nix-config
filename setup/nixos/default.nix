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
    cifs-utils
    file
    firefox
    dwarf-fortress-packages.dwarf-fortress-full
    inotify-tools
    mkpasswd
    nethack
    scrot
    seafile-shared
    signal-desktop
    unzip
    yubioath-desktop
  ];

  security.wrappers = {
    "mount.cifs".source = "${pkgs.cifs-utils}/bin/mount.cifs";
  };

  programs.steam.enable = true;
}
