{ pkgs, ... }:

{
  imports = [
    <dotfiles/setup/common>
    ./wm.nix
  ];

  environment.systemPackages = with pkgs; [
    bitwarden
    brightnessctl
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
}
