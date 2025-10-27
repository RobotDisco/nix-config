{
  config,
  lib,
  pkgs,
  ...
}:
lib.mkMerge [
  {
    services.kanshi = {
      enable = pkgs.stdenv.isLinux;
      settings = [
        {
          profile = {
            name = "clamshell";
            outputs = [
              {
                criteria = "eDP-1";
                status = "enable";
                mode = "2256x1504";
                position = "0,0";
                scale = 1.566667;
              }
              {
                criteria = "Dell Inc. DELL U2412M M2GCR1CS0T1L";
                status = "enable";
                mode = "1920x1200";
                position = "0,1504";
              }
              {
                criteria = "Dell Inc. DELL U2412M HT5N364F0GSS";
                status = "enable";
                mode = "1920x1200";
                position = "1920,1504";
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
                status = "enable";
                mode = "2256x1504";
                position = "0,0";
                scale = 1.566667;
              }
            ];
          };
        }
      ];
    };
  }
  (lib.mkIf config.wayland.windowManager.hyprland.systemd.enable {
    services.kanshi.systemdTarget = "hyprland-session.target";
  })
]
