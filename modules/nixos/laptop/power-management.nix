{
  # The daemon that handles things like power actions on lid close, and power
  # actions on low battery.
  # By default it uses hybrid sleep on critical power actions. By the time we
  # hit critical power I want the machine to hibernate and stay off.
  services.upower = {
    enable = true;

    criticalPowerAction = "Hibernate";

    percentageLow = 15;
    percentageCritical = 12;
    percentageAction = 10;
  };
  # System-level laptop power management (currently AMD prefers power-profile-daemon)
  # services.tlp.enable = true;
  # Temperature management for Intel systems
  # services.thermald.enable = true;

  # hybrid-sleep dumps the memory to disk but stays in a sleep mode until the
  # battery runs out. This way you will return to your state when charged. Use
  # if losing power is fine but you want the quickest response.
  #
  # suspend-then-hibernate sleeps until a timer runs out and then
  # hibernates. This allows for a window of waking quickly before preferring
  # maximum power savings.
  services.logind = {
    lidSwitch = "suspend-then-hibernate";
  };
  systemd.sleep.extraConfig = "HibernateDelaySec=20m";

  # Enable powersaving in network manager.
  networking.networkmanager.wifi.powersave = true;
}
