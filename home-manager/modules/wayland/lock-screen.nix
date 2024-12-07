{
  config,
  lib,
  pkgs,
  ...
}:

let
  brightnessctl = "${pkgs.brightnessctl}/bin/brightnessctl";
  hyprctl = "${pkgs.hyprland}/bin/hyprctl";

  cfg = config.robot-disco.wayland.lock-screen;
in
{
  options.robot-disco.wayland.lock-screen.enable = lib.mkEnableOption "lock screen";

  config = lib.mkIf cfg.enable {
    # Lock screen; when session is locked, require your password be typed in.
    programs.hyprlock = {
      enable = true;

      # Ugh, We have to declare the layout of the lock screen in a declarative
      # way.
      settings = {
        background = {
          path = toString ../../../backgrounds/yotsugi_eyes.png;
        };

        label = {
          text = ''"Yay, peace peace."'';
          color = "rgba(0, 230, 0, 1.0)";
          font_size = 72;
          position = "500, 210";
          halign = "center";
          valign = "bottom";
        };

        input-field = {
          hide_input = true;
          size = "600, 140";
          position = "-15, 0";
          outline_thickness = 8;
        };
      };
    };

    # Software to automatically lock screen when laptop is idle and unlocked.
    services.hypridle = {
      enable = true;

      settings = {
        general = {
          # avoid starting multiple hyprlocks.
          lock_cmd = "pidof hyprlock || hyprlock";
          # Lock session before going to sleep
          before_sleep_cmd = "loginctl lock-session";
          # Turn on monitor to avoid having to tap keyboard multiple times to
          # activate display.
          unlock_cmd = "${hyprctl} dispatch dpms on";
        };

        listener = [
          # Dim brightness after a two minutes of idleness
          {
            timeout = 120;
            # On lock, set brightness to minimum.
            on-timeout = "${brightnessctl} -s set 10}";
            # On unlock, set background back to previous setting
            on-resume = "${brightnessctl} -r";
          }
          # Lock screen after five minutes of idleness
          {
            timeout = 300;
            on-timeout = "loginctl lock-session";
          }
          # Turn off screen after fifteen minutes of idleness
          {
            timeout = 900;
            # On trigger, disable monitors
            on-timeout = "${hyprctl} dispatch dpms off";
            # On resumption, enable monitors
            on-resume = "${hyprctl} dispatch dpms on";
          }
          # Suspend/hibernate after thirty minutes of idleness.
          {
            timeout = 1800;
            on-timeout = "systemctl suspend";
          }
        ];
      };
    };
  };
}
