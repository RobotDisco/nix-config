{ config, lib, ... }:
lib.mkMerge [
  {
    services.kanshi = {
      enable = true;
      systemdTarget = "hyprland-session.target";
      settings = [
        {
          profile = {
            name = "docked";
            outputs = [
              {
                criteria = "eDP-1";
                status = "disable";
              }
              {
                criteria = "Dell Inc. DELL U2412M M2GCR1CS0T1L";
                mode = "1920x1200";
                position = "0,0";
              }
              {
                criteria = "Dell Inc. DELL U2412M HT5N364F0GSS";
                mode = "1920x1200";
                position = "1920,0";
                transform = "270";
              }
            ];
          };
        }
        {
          profile = {
            name = "roaming";
            outputs = [
              {
                criteria = "eDP-1";
                # Because I explicitly disable in some configurations, I have to
                # enable when I want it back.
                status = "enable";
                scale = 1.566667;
              }
            ];
          };
        }
      ];
    };
  }
  (lib.mkIf config.wayland.windowManager.hyprland.enable {
    services.kanshi.systemdTarget = "hyprland-session.target";
  })
]
