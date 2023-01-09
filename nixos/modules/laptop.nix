{ config, lib, pkgs, ... }:

let cfg = config.robot-disco.laptop;

in {
  options.robot-disco.laptop = {
    enable = lib.mkEnableOption "Enable laptop functionality";
  };

  config = lib.mkIf cfg.enable {
    # User-level access to power management?
    services.upower.enable = true;
    # Currently use PowerOff as ZFS doesn't support Hibernate
    services.upower.criticalPowerAction = "PowerOff";
    # System-level laptop power management
    services.tlp.enable = true;
    # Temperature management
    services.thermald.enable = true;

    # Set up hybrid sleep on idle and lid close
    # suspend, then hibernate eventually
    services.logind = {
      lidSwitch = "hybrid-sleep";
      extraConfig = ''
        HandleSuspendKey=hybrid-sleep
        IdleAction=hybrid-sleep
        IdleActionSec=20
      '';
    };

    # Use two/three finger clicks for right/middle clicks.
    services.xserver.libinput.touchpad.clickMethod = "clickfinger";

    # If network manager is used, enable power saving
    # See https://discourse.nixos.org/t/what-does-mkdefault-do-exactly/9028
    # for how mkDefault works
    networking.networkmanager = {
      enable = true;
      wifi.powersave = true;
    };

    powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";

    # Disable laptop's touchpad tap-to-click functionality
    services.xserver.libinput.touchpad.tapping = false;

    # Automatically mount dynamically removable media
    services.udisks2.enable = true;

    # Assume we need monitor hotplug support
    services.autorandr = { enable = true; };

    # Assume we're using a SSD and enable periodic SSD trim
    services.fstrim.enable = true;

    services.blueman.enable = true;

    # Enable screen locking in X
    programs.xss-lock = {
      enable = true;

      lockerCommand = "${pkgs.i3lock}/bin/i3lock -c 746542";
    };
  };
}
