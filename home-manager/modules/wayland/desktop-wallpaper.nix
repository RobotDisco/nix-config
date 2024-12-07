{ config, lib, ... }:

let
  landscape-background = toString ../../../backgrounds/frieren_white.jpg;
  portrait-background = toString ../../../backgrounds/lordran.jpg;

  cfg = config.robot-disco.wayland.wallpaper;
in
{
  options.robot-disco.wayland.wallpaper.enable = lib.mkEnableOption "Wallpaper service";

  config = lib.mkIf cfg.enable {
    services.hyprpaper = {
      enable = true;

      settings = {
        ipc = "on";
        splash = false;
        splash_offset = 2.0;

        preload = [
          landscape-background
          portrait-background
        ];

        wallpaper = [
          "eDP-1,${landscape-background}"
          "desc:Dell Inc. DELL U2412M M2GCR1CS0T1L, ${landscape-background}"
          "desc:Dell Inc. DELL U2412M HT5N364F0GSS, ${portrait-background}"
        ];
      };
    };
  };
}
