# Framework module laptop
{ config, pkgs, lib, ... }:

{
  ## Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  imports = [
      ./hardware-configuration.nix
  ];

  networking.hostName = "arrakis";
  # Enable network manager for dynamic network configuration
  networking.networkmanager = {
    enable = true;
    wifi.powersave = true;
  };

  time.timeZone = "America/Toronto";

  networking.useDHCP = false;
  networking.interfaces.wlp170s0.useDHCP = true;

  i18n.defaultLocale = "en_CA.UTF-8";

  # Fix font sizes in X
  services.xserver.dpi = 200;

  # Fix sizes of GTK/GNOME ui elements
  environment.variables = {
    GDK_SCALE = lib.mkDefault "2";
    GDK_DPI_SCALE = lib.mkDefault "0.5";
  };

  system.stateVersion = "21.11";

  boot.initrd.kernelModules = [ "vfat" "nls_cp437" "nls_iso8859-1" "usbhid" ];

  boot.initrd.luks.yubikeySupport = true;

  boot.initrd.luks.devices = {
    cryptdata = {
      device = "/dev/nvme0n1p2";
      preLVM = true;
      allowDiscards = true;
      bypassWorkqueues = true;
      yubikey = {
        slot = 2;
        twoFactor = true;
        storage = {
          device = "/dev/nvme0n1p1";
        };
      };
    };
  };

  # boot.kernelPackages = pkgs.linuxPackages_latest;
  services.fprintd.enable = true;

  # Disable laptop's touchpad tap-to-click functionality
  services.xserver.libinput.touchpad.tapping = false;

  nix = {
    # Enable nix flakes
    package = pkgs.nixFlakes;
    extraOptions = ''
        experimental-features = nix-command flakes
    '';

    # Enable binary cache downloads of standard nix packages
    binaryCaches = [
      "https://nix-community.cachix.org"
    ];
    binaryCachePublicKeys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
  };
}