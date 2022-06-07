# My Framework laptop
{ config, pkgs, lib, ... }:

let
  name = "Gaelan D'costa";
  username = "gaelan";
in {
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

  users.users."${username}" = {
    description = name;
    isNormalUser = true;
    home = "/home/gaelan";
    group = "users";
    createHome = true;
    extraGroups = [ "networkmanager" "wheel" ];
    openssh.authorizedKeys.keyFiles = [ ./gaelan-yubikey.pub ];
    shell = pkgs.zsh;
  };

  # Managed home directories
  home-manager.users."${username}" = { robot-disco.user.gaelan.enable = true; };

  nix.settings.trusted-users = [ "root" username ];

  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 30d";
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

  services.fwupd.enable = true;

  # Since this server is full-disk-encrypted, just automatically log in
  services.xserver.desktopManager.session = [{
    name = "home-manager";
    start = ''
      ${pkgs.runtimeShell} $HOME/.xsession &
      waitPID = $!
    '';
  }];
  services.xserver.displayManager.autoLogin = {
    enable = true;
    user = "gaelan";
  };

  services.logind = {
    lidSwitch = "hybrid-sleep";
    extraConfig = ''
      HandleSuspendKey=hybrid-sleep
      IdleAction=hybrid-sleep
      IdleActionSec=20
    '';
  };

  # rtkit is optional for sound but recommended for some reason
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    # ALSA is the low-level audio layer of Linux.
    alsa.enable = true;
    alsa.support32Bit = true;
    # Enable PulseAudio compatibility
    pulse.enable = true;
 };

 services.upower.enable = true;
 services.tlp.enable = true;
 
}
