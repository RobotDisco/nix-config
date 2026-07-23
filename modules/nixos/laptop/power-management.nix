{ config, lib, ... }:
let
  inherit (lib) mkIf;
  cfg = config.robot-disco.power-management;
in
{
  options.robot-disco.power-management = {
    enableAMD = lib.mkEnableOption "AMD Power Management";
    enableIntel = lib.mkEnableOption "Intel Power Management";
  };

  config = lib.mkMerge [
    {
      # Only one of AMD and Intel Power Management can be enabled.
      assertions = [
        {
          assertion = !(cfg.enableAMD && cfg.enableIntel);
          message = "Enable only one of 'enableAMD' or 'enableIntel'.";
        }
      ];
    }
    (mkIf cfg.enableAMD {
      services = {
        # Framework / AMD strongly encourage PPD over TLP.
        power-profiles-daemon.enable = true;

        # Disable TLP and the Intel-only thermald.
        tlp.enable = false;
        thermald.enable = false;
      };
    })
    (mkIf cfg.enableIntel {
      services = {
        # TLP is more featureful than PPD, so prefer it for Intel.
        tlp.enable = true;
        # Temperature management for Intel systems
        thermald.enable = true;

        # Disable PPD in favour of TLP.
        power-profiles-daemon.enable = false;
      };
    })
    {
      services = {
        upower = {
          # The daemon that desktop environments talk to to get battery
          # information. It also guides them on what to do when the battery is
          # critically low.
          # By default it requests hybrid sleep on critical power actions. By the
          # time we hit critical power I want the machine to hibernate and stay
          # off.
          enable = true;

          criticalPowerAction = "Hibernate";

          percentageLow = 50;
          percentageCritical = 45;
          percentageAction = 40;
        };
        # Logind governs what happens when the laptop lid is closed.
        #
        # hybrid-sleep dumps the memory to disk but stays in a sleep mode until the
        # battery runs out. This way you will return to your state when charged. Use
        # if losing power is fine but you want the quickest response.
        #
        # suspend-then-hibernate sleeps until a timer runs out and then
        # hibernates. This allows for a window of waking quickly before preferring
        # maximum power savings.
        logind.settings.Login.HandleLidSwitch = "suspend-then-hibernate";
      };

      # Sleep for 20 minutes, then hibernate.
      systemd.sleep.settings.Sleep.HibernateDelaySec = "20m";

      # Enable wireless powersaving in network manager.
      networking.networkmanager.wifi.powersave = true;
    }
  ];
}
