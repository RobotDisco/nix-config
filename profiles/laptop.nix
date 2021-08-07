{ pkgs, ... }:

{
  # Enable network manager for dynamic network configuration
  networking.networkmanager = {
    enable = true;
    wifi.powersave = true;
  };

  # If we use bluetooth, support nice bluetooth audio codecs
  hardware.pulseaudio = {
    # Need full pulseaudio package for bluetooth audio
    package = pkgs.pulseaudioFull;

    # Want good audio codecs (APTX, LDAC) not garbage SBC
    extraModules = [ pkgs.pulseaudio-modules-bt ];
  };

  # Set up a power management daemon
  services.upower.enable = true;
  # Enable linux power management
  services.tlp.enable = true;

  # Support monitor hotplugging
  services.autorandr.enable = true;

  # Enable bluetooth
  hardware.bluetooth.enable = true;
  # Enable a nice bluetooth manager GUI
  services.blueman.enable = true;

  services.logind.lidSwitch = "hybrid-sleep";

  # Support dynamically removable media
  services.udisks2.enable = true;
}
