{
  # The daemon that handles things like power actions on lid close, and power
  # actions on low battery.
  # By default it uses hybrid sleep on critical power actions.
  services.upower = {
    enable = true;

    percentageCritical = 5;
    percentageAction = 4;
  };
  # System-level laptop power management
  services.tlp.enable = true;
  # Temperature management
  services.thermald.enable = true;

  # Set up hybrid sleep on idle and lid close
  # suspend, then hibernate eventually
  services.logind = {
    lidSwitch = "hybrid-sleep";
  };

  # Enable powersaving in network manager.
  networking.networkmanager.wifi.powersave = true;
}
