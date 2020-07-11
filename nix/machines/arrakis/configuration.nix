# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, lib, pkgs, ... }:

let
  secrets = import ./secrets.nix;
in
{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix

      <home-manager/nixos>
      (import ../../users/gaelan { inherit config; inherit pkgs; linux = true; })
    ];

  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
    }))
  ];

  # I'm fine installing non-free software.
  nixpkgs.config.allowUnfree = true;

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # networking.hostName = "nixos"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.enp0s31f6.useDHCP = true;
  networking.interfaces.wlp4s0.useDHCP = true;

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  i18n = {
  #   consoleFont = "Lat2-Terminus16";
  #   consoleKeyMap = "us";
    defaultLocale = "en_CA.UTF-8";
  };

  # Use the same keyboard settings for console as X11
  console.useXkbConfig = true;

  # Set your time zone.
  time.timeZone = "America/Toronto";

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    brightnessctl
    mkpasswd
    vim
    links2

    # chefdk
    dosbox
    file
    pavucontrol
    seafile-shared
    inotify-tools
    libsearpc
    timidity
    # Fluid3
    pmidi
    transmission
    vlc
    # wine-wow
    chromium
    firefox
    git
    emacs
    bitwarden
    bitwarden-cli
    yubioath-desktop
    signal-desktop
    obs-studio
    dwarf-fortress-packages.dwarf-fortress-full
    unzip
    zsh
    keychain
    xmobar
    scrot
    steam
    networkmanager_l2tp
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = { enable = true; enableSSHSupport = true; };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Enable CUPS to print documents.
  services.printing.enable = true;

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
  services.xserver.xkbOptions = "ctrl:nocaps";
  #services.xserver.xkbVariant = "colemak";
  
  # Enable touchpad support.
  services.xserver.libinput.enable = true;
  services.xserver.libinput.tapping = false;

  # Enable the KDE Desktop Environment.
  # services.xserver.displayManager.sddm.enable = true;
  # services.xserver.desktopManager.plasma5.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  # users.users.jane = {
  #   isNormalUser = true;
  #   extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
  # };
  users.users.root.hashedPassword = secrets.userRootPasswordHash;
  users.users.gaelan = {
    isNormalUser = true;
    home = "/home/gaelan";
    description = "Gaelan D'costa";
    extraGroups = [ "wheel" "networkmanager" "scanner" "video" "input" "docker" ];
    hashedPassword = secrets.userGaelanPasswordHash;
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "19.09"; # Did you read the comment?

  boot.supportedFilesystems = [ "zfs" ];

  networking.hostName = "arrakis";
  networking.hostId = "9cc13837";

  networking.networkmanager.enable = true;

  users.mutableUsers = false;

  system.autoUpgrade.enable = true;

  services.tlp.enable = true;

  services.udev.packages = [ pkgs.yubikey-personalization pkgs.libu2f-host ];
  services.pcscd.enable = true;

  fonts = {
    enableDefaultFonts = true;
    fonts = [
      pkgs.anonymousPro
      pkgs.camingo-code
    ];
  };

  hardware.enableRedistributableFirmware = true;
  hardware.cpu.intel.updateMicrocode = true;

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
  nixpkgs.config.packageOverrides = pkgs: {
    sedutil = (pkgs.sedutil.overrideAttrs (oldAttrs: {
      patches = (oldAttrs.patches or []) ++ [
        # Add support for enabling unlocking when resuming from sleep
        # See: https://github.com/Drive-Trust-Alliance/sedutil/pull/190
        (builtins.fetchurl {
          url = https://patch-diff.githubusercontent.com/raw/Drive-Trust-Alliance/sedutil/pull/190.patch; 
          sha256 = "c0618a319eb0c9a6efe9c72db59338232b235079042ccf77b1d690f64f735a42";
        })
      ];
    }));
  };

  # NOTE: Generate the password hash with: sudo sedutil-cli --printPasswordHash 'plaintext-password-here' /dev/nvme0n1  
  systemd.services.sedutil-s3sleep = {
    description = "Enable S3 sleep on OPAL self-encrypting drives";
    documentation = [ "https://github.com/Drive-Trust-Alliance/sedutil/pull/190" ];
    path = [ pkgs.sedutil ];
    script = "sedutil-cli -n -x --prepareForS3Sleep 0 ${secrets.opalDiskPasswordHash} /dev/nvme0n1";
    wantedBy = [ "multi-user.target" ];
  };
  # Sleep
  #   https://www.kernel.org/doc/html/latest/admin-guide/pm/sleep-states.html#basic-sysfs-interfaces-for-system-suspend-and-hibernation
  #   Force hybrid-sleep on suspend:
  #     - When suspending, write RAM to disk (hibernate)
  #     - When done writing hibernation image, suspend.
  environment.etc."systemd/sleep.conf".text = pkgs.lib.mkForce "
    [Sleep]
    SuspendState=disk
    SuspendMode=suspend
  ";

  programs.zsh.enable = true;

  users.defaultUserShell = pkgs.zsh;

  services.zfs.autoScrub.enable = true;
  services.zfs.trim.enable = true;

  services.upower.enable = true;

  hardware.opengl.driSupport32Bit = true;
  hardware.opengl.extraPackages32 = with pkgs.pkgsi686Linux; [ libva ];
  hardware.pulseaudio.support32Bit = true;

  hardware.bluetooth.enable = true;

  services.xserver.windowManager.session = lib.singleton {
    name = "xsession";
    start = pkgs.writeScript "xsession" ''
      #!${pkgs.runtimeShell}
      if test -f $HOME/.xsession; then
        exec ${pkgs.runtimeShell} -c $HOME/.xsession
      else
        echo "No xsession script found"
        exit 1
      fi
    '';
  };

  services.autorandr.enable = true;

  fonts.fontconfig.enable = true;

  systemd.user.services."seafile-cli" = {
    after = [ "network.target" ];
    enable = true;
    description = "Seafile CLI Client";
    wantedBy = [ "default.target" ];
    path = [ pkgs.seafile-shared ];
    serviceConfig.Type = "oneshot";
    serviceConfig.ExecStart = "${pkgs.seafile-shared}/bin/seaf-cli start";
    serviceConfig.ExecStop = "${pkgs.seafile-shared}/bin/seaf-cli stop";
    serviceConfig.RemainAfterExit="yes";
  };
}
