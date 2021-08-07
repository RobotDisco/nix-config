# Configuration for laptops
{pkgs, ... }:

{
  import = [
    ./audio.nix
    ./common.nix
    ./hardware/home-devices.nix
    ./networking/smb-client.nix
    ./software/steam.nix
  ];

  environment.systemPackages = with pkgs; [
    # Control laptop screen brightness
    brightnessctl
  ];
    
  # Enable network manager for dynamic network configuration
  networking.networkmanager = {
    enable = true;
    wifi.powersave = true;
  };

  
  hardware = {
    bluetooth.enable = true;
    
    pulseaudio = {
      # Need full pulseaudio package for bluetooth audio
      package = pkgs.pulseaudioFull;
      # Want good audio codecs (APTX, LDAC) not garbage SBC
      extraModules = [ pkgs.pulseaudio-modules-bt ];
    };
  };

  services = {
    # Monitor hotplugging
    autorandr.enable = true;

    # Enable a nice bluetooth manager GUI
    blueman.enable = true;

    # Use hybrid-sleep when closing laptop lid
    logind.lidSwitch = "hybrid-sleep";
    
    # Power management daemon
    tlp.enable = true;
    # Allow applications to access power management info
    upower.enable = true;

    # Automatically mount dynamically removable media
    udisks2.enable = true;

    # Disable laptop's touchpad tap-to-click functionality
    xserver.libinput.touchpad.tapping = false;
  };
}
