# My Framework laptop
{ config, pkgs, lib, ... }:

{
  networking.hostName = "arrakis";
  networking.interfaces.wlp170s0.useDHCP = true;
  networking.useDHCP = false;

  boot.initrd.availableKernelModules =
    [ "xhci_pci" "thunderbolt" "nvme" "usb_storage" "usbhid" "sd_mod" ];
  boot.kernelModules = [ "dm-snapshot" ];
  boot.kernelParams = [ "mem_sleep_default=deep" ];
  boot.kernelPackages = pkgs.linuxPackages_latest;

  nix.maxJobs = lib.mkDefault "auto";
  system.stateVersion = "22.05";

  fileSystems."/" = {
    device = "/dev/disk/by-label/rootpart0";
    fsType = "ext4";
    options = [ "noatime" ];
  };

  fileSystems."/home" = {
    device = "/dev/disk/by-label/homepart0";
    fsType = "ext4";
    options = [ "noatime" ];
  };

  fileSystems."/nix" = {
    device = "/dev/disk/by-label/nixpart0";
    fsType = "ext4";
    options = [ "noatime" ];
  };

  fileSystems."/var" = {
    device = "/dev/disk/by-label/varpart0";
    fsType = "ext4";
    options = [ "relatime" ];
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-label/EFIBOOT0";
    fsType = "vfat";
  };

  swapDevices = [{ device = "/dev/disk/by-label/swappart0"; }];

  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
  hardware.cpu.intel.updateMicrocode =
    lib.mkDefault config.hardware.enableRedistributableFirmware;
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
        storage = { device = "/dev/nvme0n1p1"; };
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

  services.xserver = {
    enable = true;
    windowManager.exwm.enable = true;
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
    binaryCaches = [ "https://nix-community.cachix.org" ];
    binaryCachePublicKeys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
  };

  boot.kernel.sysctl = { "vm.swappiness" = 1; };

  services.fstrim.enable = true;

  #nixpkgs.config.packageOverrides = pkgs: {
  # vaapiIntel = pkgs.vaapiIntel.override { enableHybridCodec = true; };
  #};

  #hardware.opengl = {
  #  enable = true;
  #  extraPackages = with pkgs; [
  #    intel-media-driver
  #    vaapiIntel
  #    vaapiVdpau
  #    libvdpau-va-gl
  #  ];
  #  extraPackages32 = with pkgs.pkgsi686Linux; [ vaapiIntel ];
  #}

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
    description = "Gaelan D'costa";
    extraGroups = [ "wheel" ];
    openssh.authorizedKeys.keyFiles = [ ./gaelan-yubikey.pub ];
  };

  services = {
    # Enable smart card (CCID) mode
    pcscd.enable = true;

    # udev rules often needed for yubikey support
    udev.packages = [ pkgs.yubikey-personalization ];
  };

  # Enable yubikey as a way to login
  security.pam.yubico = {
    enable = true;
    # Use a local challenge-response, not yubico's cloud service
    mode = "challenge-response";
    # TODO should I define user "gaelan"'s yubikey here?
  };

  home-manager.users.gaelan = {
    # Set up some reasonble and secure ssh configuration
    programs.ssh = {
      enable = true;
      compression = true;
      # Don't forward by default, it is insecure
      # Prefer proxyjumping if you can
      forwardAgent = false;
    };

    # install and configure git
    programs.git = {
      enable = true;
      extraConfig = {
        core = { autocrlf = "input"; };
        hub = { protocol = "https"; };
      };

      # TODO write a tulip nix-shell that sets these to tulip email addresses.
      userEmail = "gdcosta@gmail.com";
      userName = "Gaelan D'costa";
    };

    programs.gpg = {
      enable = true;
      settings = {
        personal-cipher-preferences = "AES256 AES192 AES";
        personal-digest-preferences = "SHA512 SHA384 SHA256";
        personal-compress-preferences = "ZLIB BZIP2 ZIP Uncompressed";
        default-preference-list =
          "SHA512 SHA384 SHA256 AES256 AES192 AES ZLIB BZIP2 ZIP Uncompressed";
        cert-digest-algo = "SHA512";
        s2k-digest-algo = "SHA512";
        s2k-cipher-algo = "AES256";
        charset = "utf-8";
        fixed-list-mode = true;
        no-comments = true;
        no-emit-version = true;
        no-greeting = true;
        keyid-format = "0xlong";
        list-options = "show-uid-validity";
        verify-options = "show-uid-validity";
        with-fingerprint = true;
        require-cross-certification = true;
        no-symkey-cache = true;
        use-agent = true;
        throw-keyids = true;
      };
    };

    services.gpg-agent = {
      enable = true;
      enableExtraSocket = true;
      enableSshSupport = true;
      extraConfig = ''
        allow-emacs-pinentry
        allow-loopback-pinentry
      '';
      defaultCacheTtl = 60;
      maxCacheTtl = 120;
    };

    # Document these
    programs.zsh = { enable = true; };
  };
}
