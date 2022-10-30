{ lib, config, pkgs, ... }:

let
  cfg = config.robot-disco.hardware.framework;
in

{
  options.robot-disco.hardware.framework = {
    enable = lib.mkEnableOption "Enable support for Framework Laptops";
  };

  config = lib.mkIf cfg.enable {
    # Boot level config for Framework laptops
    # This is also included in nix-hardware but I feel
    # more comfortable also including it explicitly.
    boot.initrd.availableKernelModules =
      [ "xhci_pci" "thunderbolt" "nvme" "usb_storage" "usbhid" "sd_mod" ];
    boot.kernelModules = [ "dm-snapshot" ];
    boot.kernelParams = [ "mem_sleep_default=deep" ];
    boot.kernelPackages = lib.mkDefault pkgs.linuxPackages_latest;

    # Enable the Framework networking device
    networking.interfaces.wlp170s0.useDHCP = true;

    # The framework has a 3:2 high-resolution display.
    hardware.video.hidpi.enable = true;

    # Fix font sizes in X
    services.xserver.dpi = 200;

    # Fix sizes of GTK/GNOME ui elements
    environment.variables = {
      GDK_SCALE = "2";
      GDK_DPI_SCALE = "0.5";
    };

    # Enable the Framework's fingerprint reader
    services.fprintd.enable = true;
  };
}
