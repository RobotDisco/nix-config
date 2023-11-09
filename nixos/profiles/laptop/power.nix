{
  # User-level access to read power management info?
  services.upower.enable = false;
  # System-level laptop power management
  services.tlp.enable = true;
  # Temperature management
  services.thermald.enable = true;

  # Set up hybrid sleep on idle and lid close
  # suspend, then hibernate eventually
  services.logind = {
    lidSwitch = "hybrid-sleep";
  };

  # Don't know if I need this, TPM might handle this for me
  # powerManagement.cpuFreqGovernor = "powersave";

  # Enable powersaving in network manager.
  networking.networkmanager.wifi.powersave = true;
}
