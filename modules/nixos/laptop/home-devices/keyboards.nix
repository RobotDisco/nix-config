{ pkgs, ... }:

{
  # Enable support for QMK keyboards like my Megalodon
  hardware.keyboard.qmk.enable = true;

  # One of my workhorse keyboards has configuration software.
  hardware.keyboard.uhk.enable = true;

  # Install configuration software for my keyboards
  environment.systemPackages = [
    pkgs.uhk-agent
    pkgs.vial
  ];
}
