{ lib, ... }:

{
  # User-level access to power management?
  services.upower.enable = true;
  # System-level laptop power management
  services.tlp.enable = true;

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
}