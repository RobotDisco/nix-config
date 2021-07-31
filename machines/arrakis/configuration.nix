# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, ... }:

let

username = "gaelan";
hostName = "arrakis";

in

{
  nixpkgs.overlays =
    let path = <dotfiles/overlays>; in with builtins;
      map (n: import (path + ("/" + n)))
          (filter (n: match ".*\\.nix" n != null ||
	              pathExists (path + ("/" + n + "/default.nix")))
		  (attrNames (readDir path)));

  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      <dotfiles/setup/nixos>
      <dotfiles/home-manager/nixos>
      ./cachix.nix
    ];

  nix.nixPath = [
    "nixos-config=/etc/nixos/configuration.nix"
    "nixpkgs=/nix/var/nix/profiles/per-user/root/channels/nixos"
    "dotfiles=/home/gaelan/code/dotfiles"
    "/nix/var/nix/profiles/per-user/root/channels"
  ];
  nix.package = pkgs.nixFlakes;
  nix.extraOptions = ''
    experimental-features = nix-command flakes
  '';

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = hostName; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  #networking.interfaces.enp0s31f6.useDHCP = true;
  #networking.interfaces.wlp4s0.useDHCP = true;

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_CA.UTF-8";
  # console = {
  #   font = "Lat2-Terminus16";
  #   keyMap = "us";
  # };
  console.useXkbConfig = true;

  # Set your time zone.
  time.timeZone = "America/Toronto";

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  # environment.systemPackages = with pkgs; [
  #   wget vim
  # ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  #   pinentryFlavor = "gnome3";
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  sound.enable = true;

  hardware.pulseaudio = {
    enable = true;
    
    # Need full pulseaudio package for bluetooth audio
    package = pkgs.pulseaudioFull;

    # Want good audio codecs (APTX, LDAC) not garbage SBC
    extraModules = [ pkgs.pulseaudio-modules-bt ];
  };

  # Enable the X11 windowing system.
  services.xserver.enable = true;
  # services.xserver.layout = "us";
  services.xserver.xkbOptions = "ctrl:nocaps";

  # Enable touchpad support.
  services.xserver.libinput.enable = true;
  services.xserver.libinput.touchpad.tapping = false;
  # Remap buttons for my kensington trackball
  services.xserver.libinput.mouse = {
    dev = "/dev/input/by-id/usb-047d_Kensington_Expert_Mouse-mouse";
    buttonMapping = "1 2 8 4 5 6 7 3";
  };

  # Enable the KDE Desktop Environment.
  # services.xserver.displayManager.sddm.enable = true;
  # services.xserver.desktopManager.plasma5.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  # users.users.jane = {
  #   isNormalUser = true;
  #   extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
  # };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.05"; # Did you read the comment?

  boot.supportedFilesystems = [ "zfs" ];

  networking.hostId = "140f9be5";

  networking.networkmanager = {
    enable = true;
    enableStrongSwan = true;
    wifi.powersave = true;
  };

  users.users.gaelan = {
    isNormalUser = true;
    home = "/home/gaelan";
    description = "Gaelan D'costa";
    extraGroups = [ "wheel" "networkmanager" "docker" "video" ];
  };

  home-manager.users = {
    gaelan = import <dotfiles/setup/user>;
  };

  services.tlp.enable = true;

  services.autorandr.enable = true;

  # Enable CUPS to print documents.
  services.printing.enable = true;

  hardware.sane.enable = true;
  nixpkgs.config.sane.extraFirmware = [
    {
      src = pkgs.fetchurl {
        # http://www.openfusion.net/linux/scansnap_1300i
        url = "http://www.openfusion.net/public/files/1300i_0D12.nal";
        sha256 = "cbea48c6cee675c2ea970944b49b805d665ee659f753a50b83c176973f507591";
      };
      name = "1300i_0D12.nal";
      backend = "epjitsu";
    }
  ];

  hardware.trackpoint.enable = true;
  hardware.trackpoint.emulateWheel = true;

  programs.sedutil.enable = true;
  programs.slock.enable = true;

  # Self-encrypting drive (OPAL)
  # nixpkgs.config.packageOverrides = pkgs: {
  #  sedutil = (pkgs.sedutil.overrideAttrs (oldAttrs: {
  #     patches = (oldAttrs.patches or []) ++ [
  #       # Add support for enabling unlocking when resuming from sleep
  #       # See: https://github.com/Drive-Trust-Alliance/sedutil/pull/190
  #       (builtins.fetchurl {
  #         url = https://patch-diff.githubusercontent.com/raw/Drive-Trust-Alliance/sedutil/pull/190.patch; 
  #         sha256 = "c0618a319eb0c9a6efe9c72db59338232b235079042ccf77b1d690f64f735a42";
  #       })
  #     ];
  #   }));
  # };

  # NOTE: Generate the password hash with: sudo sedutil-cli --printPasswordHash 'plaintext-password-here' /dev/nvme0n1  
  # systemd.services.sedutil-s3sleep = {
  #   description = "Enable S3 sleep on OPAL self-encrypting drives";
  #   documentation = [ "https://github.com/Drive-Trust-Alliance/sedutil/pull/190" ];
  #   path = [ pkgs.sedutil ];
  #   script = "sedutil-cli -n -x --prepareForS3Sleep 0 ${secrets.opalDiskPasswordHash} /dev/nvme0n1";
  #   wantedBy = [ "multi-user.target" ];
  # };
  # # Sleep
  # #   https://www.kernel.org/doc/html/latest/admin-guide/pm/sleep-states.html#basic-sysfs-interfaces-for-system-suspend-and-hibernation
  # #   Force hybrid-sleep on suspend:
  # #     - When suspending, write RAM to disk (hibernate)
  # #     - When done writing hibernation image, suspend.
  # environment.etc."systemd/sleep.conf".text = pkgs.lib.mkForce "
  #   [Sleep]
  #   SuspendState=disk
  #   SuspendMode=suspend
  # ";

  users.defaultUserShell = pkgs.zsh;

  services.zfs.autoScrub.enable = true;
  services.zfs.trim.enable = true;

  services.upower.enable = true;

  hardware.bluetooth.enable = true;

  services.fwupd.enable = true;
  services.logind.lidSwitch = "hybrid-sleep";

  services.udev.extraRules = ''
# Initialise Apple SuperDrive
ACTION=="add", ATTRS{idProduct}=="1500", ATTRS{idVendor}=="05ac", DRIVERS=="usb", RUN+="${pkgs.sg3_utils}/bin/sg_raw /dev/$kernel EA 00 00 00 00 00 01"
'';
  services.pcscd.enable = true;

  hardware.enableRedistributableFirmware = true;
  hardware.cpu.intel.updateMicrocode = true;

  virtualisation.docker.enable = true;

  services.lorri.enable = true;

  services.hardware.bolt.enable = true;
}
