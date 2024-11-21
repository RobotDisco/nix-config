{ pkgs, ... }:

let
  brightnessctl = "${pkgs.brightnessctl}/bin/brightnessctl";
  hyprctl = "${pkgs.hyprland}/bin/hyprctl";
in
{
  # Automatically lock the display when idle
  services.hypridle = {
    enable = true;

    settings = {
      general = {
        # avoid starting multiple hyprlocks.
        lock_cmd = "pidof hyprlock || hyprlock";
        # Lock session before going to sleep
        before_sleep_cmd = "loginctl lock-session";
        # Signal DPMS to avoid having to tap keyboard multiple times
        # to activate display.
        unlock_cmd = "${hyprctl}  dispatch dpms on";
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
          on-resume = "${hyprctl} displatch dpms on";
        }
        # Turn off screen after fifteen minutes of idleness
        {
          timeout = 1800;
          on-resume = "systemctl suspend";
        }
      ];
    };
  };

  # Setup the command that locks the screens
  programs.hyprlock = {
    enable = true;

    # Ugh, We have to declare the layout of the lock screen in a declarative
    # way.
    settings = {
      label = {
        text = "Hello!";
      };
    };
  };
}
