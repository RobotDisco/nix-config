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

  # Ideally I want to use hybrid-sleep, but for some reason it doesn't work
  # hibernate, however, does.
  # so, to preserve my working state during low battery, hibernate on lid close.
  services.logind = {
    lidSwitch = "hibernate";
  };

  # Enable powersaving in network manager.
  networking.networkmanager.wifi.powersave = true;
}
