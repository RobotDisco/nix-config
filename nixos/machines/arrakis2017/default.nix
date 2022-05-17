# My Lenovo X1 Carbon, 5th Generation
{ config, pkgs, lib, inputs, ... }:

let
  hostName = "arrakis";
in

{
  networking.hostName = hostName; # Define your hostname.
  
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      # This is a laptop
      ../../profiles/laptop.nix
      ../../profiles/window-manager.nix
      ../../profiles/tulip.nix
      # Let's play games
      ../../profiles/software/steam.nix
    ];