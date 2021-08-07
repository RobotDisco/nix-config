# (Graphical) Applications for using yubikeys
{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    yubikey-manager-qt
    yubikey-personalization-gui
    yubioath-desktop
  ];
}
