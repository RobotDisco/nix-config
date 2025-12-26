{ config, lib, ... }:

let
  landscape-background = toString ../../../backgrounds/frieren_white.jpg;
  portrait-background = toString ../../../backgrounds/lordran.jpg;

  cfg = config.robot-disco.wayland.wallpaper;
in
{
  options.robot-disco.wayland.wallpaper.enable = lib.mkEnableOption "Enable wallpaper for wayland.";

  config = lib.mkIf cfg.enable {
    services.wpaperd = {
      enable = true;

      settings = {
        "default" = {
          mode = "center";
        };
        "eDP-1" = {
          path = landscape-background;
        };
        "Dell Inc. DELL U2412M M2GCR1CS0T1L" = {
          path = landscape-background;
        };
        "Dell Inc. DELL U2412M HT5N364F0GSS" = {
          path = portrait-background;
        };
      };
    };
  };
}
