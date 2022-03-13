# Framework module laptop
{ config, pkgs, lib, inputs, ... }:

{
  # enable HiDPI
  hardware.video.hidpi.enable = lib.mkDefault true;

  imports = [
      inputs.nix-hardware.framework
      ./hardware-configuration.nix
  ];

  ## Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Fix font sizes in X
  services.xserver.dpi = 200;

  # Fix sizes of GTK/GNOME ui elements
  environment.variables = {
    GDK_SCALE = lib.mkDefault "2";
    GDK_DPI_SCALE = lib.mkDefault "0.5";
  };
}