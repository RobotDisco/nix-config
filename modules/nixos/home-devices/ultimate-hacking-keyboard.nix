{ pkgs, ... }:

{
  # One of my workhorse keyboards has configuration software.
  hardware.keyboard.uhk.enable = true;

  # This is a program I need to set up to start up in my desktop environment
  # This file may wind up becoming a module at some point.
  environment.systemPackages = [
    pkgs.uhk-agent
  ];
}
