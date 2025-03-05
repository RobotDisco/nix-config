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
            }
            {
              criteria = "Dell Inc. DELL U2412M HT5N364F0GSS";
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
              scale = 1.566667;
            }
          ];
        };
      }
    ];
  };
}
