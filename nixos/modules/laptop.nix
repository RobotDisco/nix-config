{ pkgs, ... }:

{
  # User-level access to power management?
  services.upower.enable = true;
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
  # Since I use wireguard to route all traffic to my home VPN
  # we need to tell my firewall that is ok for the firewall's
  # reverse path filter to ignore wireguard traffic
  networking.firewall = {
    # log packets that are dropped
    logReversePathDrops = true;

    extraCommands = ''
      ip46tables -t mangle -I nixos-fw-rpfilter -p udp -m udp --sport 20990 -j RETURN
      ip46tables -t mangle -I nixos-fw-rpfilter -p udp -m udp --dport 20990 -j RETURN
    '';
    extraStopCommands = ''
      ip46tables -t mangle -D nixos-fw-rpfilter -p udp -m udp --sport 20990 -j RETURN || true
      ip46tables -t mangle -D nixos-fw-rpfilter -p udp -m udp --dport 20990 -j RETURN || true
    '';
  };

  # Enable avahi for chromecast support
  services.avahi.enable = true;

  powerManagement.cpuFreqGovernor = "powersave";

  # Disable laptop's touchpad tap-to-click functionality
  services.xserver.libinput.touchpad.tapping = false;

  # Automatically mount dynamically removable media
  services.udisks2.enable = true;

  # Assume we need monitor hotplug support
  services.autorandr = { enable = true; };

  # Assume we're using a SSD and enable periodic SSD trim
  services.fstrim.enable = true;

  # Enable bluetotoh
  hardware.bluetooth.enable = true;
  services.blueman.enable = true;

  # Enable screen locking in X
  programs.xss-lock = {
    enable = true;

    lockerCommand = "${pkgs.i3lock}/bin/i3lock -c 746542";
  };
}
