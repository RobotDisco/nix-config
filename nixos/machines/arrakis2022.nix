# My Framework laptop
{ config, pkgs, lib, ... }:

{ 
  networking.hostName = "arrakis";
  networking.interfaces.wlp170s0.useDHCP = true;
  networking.useDHCP = false;

  boot.initrd.availableKernelModules = [ "xhci_pci" "thunderbolt" "nvme" "usb_storage" "usbhid" "sd_mod" ];
  boot.kernelModules = [ "dm-snapshot" ];
  boot.kernelParams = [ "mem_sleep_default=deep" ];
  boot.kernelPackages = pkgs.linuxPackages_latest;

  nix.maxJobs = lib.mkDefault "auto";
  system.stateVersion = "21.11";

  fileSystems."/" =
    { device = "/dev/disk/by-label/rootpart0";
      fsType = "ext4";
      options = [ "noatime" ];
    };

  fileSystems."/home" =
    { device = "/dev/disk/by-label/homepart0";
      fsType = "ext4";
      options = [ "noatime" ];
    };

  fileSystems."/nix" =
    { device = "/dev/disk/by-label/nixpart0";
      fsType = "ext4";
      options = [ "noatime" ];
    };

  fileSystems."/var" =
    { device = "/dev/disk/by-label/varpart0";
      fsType = "ext4";
      options = [ "relatime" ];
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-label/EFIBOOT0";
      fsType = "vfat";
    };

  swapDevices = [ { device = "/dev/disk/by-label/swappart0"; } ];

  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
  # high-resolution display
  hardware.video.hidpi.enable = true;

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

  ## Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Enable network manager for dynamic network configuration
  networking.networkmanager = {
    enable = true;
    wifi.powersave = true;
  };

  time.timeZone = "America/Toronto";
  i18n.defaultLocale = "en_CA.UTF-8";

  # Fix font sizes in X
  services.xserver.dpi = 200;

  # Fix sizes of GTK/GNOME ui elements
  environment.variables = {
    GDK_SCALE = "2";
    GDK_DPI_SCALE = "0.5";
  };                

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

  boot.kernel.sysctl = {
    "vm.swappiness" = 1;
  };

  services.fstrim.enable = true;

  #nixpkgs.config.packageOverrides = pkgs: {
  # vaapiIntel = pkgs.vaapiIntel.override { enableHybridCodec = true; };
  #};

  hardware.opengl = {
    enable = true;
    extraPackages = with pkgs; [
      intel-media-driver
      vaapiIntel
      vaapiVdpau
      libvdpau-va-gl
    ];
    extraPackages32 = with pkgs.pkgsi686Linux; [ vaapiIntel ];
  };

  console = {
    # Honour the same settings as the linux console as in X11
    useXkbConfig = true;
  };
  # Caps Lock must die; replace with Ctrl
  services.xserver.xkbOptions = "ctrl:nocaps";

  nixpkgs = {
    # Enable nonfree software
    config.allowUnfree = true;
  };

  # Don't allow anyone except those in the admin group to
  # perform a sudo.
  security.sudo.execWheelOnly = true;

  users.users.gaelan = {
    shell = pkgs.zsh;
    isNormalUser = true;
    home = "/home/gaelan";
    description = "Gaelan D'costa"
    extraGroups = [ "wheel" ];
    openssh.authorizedKeys.keyFiles = [
      ./gaelan-yubikey.pub
    ];
  };
}
