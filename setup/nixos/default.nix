{ pkgs, ... }:

{
  imports = [
    <dotfiles/setup/common>
    ./wm.nix
  ];

  environment.systemPackages = with pkgs; [
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
    steam
    unzip
    yubioath-desktop
  ];

  security.wrappers = {
    "mount.cifs".source = "${pkgs.cifs-utils}/bin/mount.cifs";
  };
}
