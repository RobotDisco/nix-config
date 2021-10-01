# (Graphical) Applications for using yubikeys
{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    # Graphical
    yubikey-manager-qt
    yubikey-personalization-gui
    yubioath-desktop
    # CLI
    yubikey-manager
    yubikey-personalization
    yubico-piv-tool
  ];
}
