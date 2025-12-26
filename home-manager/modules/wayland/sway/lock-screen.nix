{
  config,
  lib,
  pkgs,
  ...
}:

let
  brightnessctl = "${pkgs.brightnessctl}/bin/brightnessctl";
  cfg = config.robot-disco.wayland.sway.lock-screen;
in

{
  # TODO: Expose timeout values (dim, lock, screen-off, suspend) as module
  # options for reuse across machines with different idle preferences.
  #
  # Note: intentionally not auto-enabled by the sway module — both sway and
  # hyprland may be active simultaneously, and only one idle daemon should
  # run at a time.
  options.robot-disco.wayland.sway.lock-screen.enable =
    lib.mkEnableOption "Enable swayidle + swaylock.";

  config = lib.mkIf cfg.enable {
    programs.swaylock = {
      enable = true;
    };

    services = {
      swayidle = {

        enable = true;

        events = [
          {
            event = "after-resume";
            command = "${brightnessctl} -r";
          }
          {
            event = "before-sleep";
            command = "loginctl lock-session";
          }
          {
            event = "lock";
            command = "pidof swaylock || swaylock";
          }
        ];

        timeouts = [
          # Dim brightness after a two minutes of idleness
          {
            timeout = 120;
            # On lock, set brightness to minimum.
            command = "${brightnessctl} -s set 10";
            # On unlock, set background back to previous setting
            resumeCommand = "${brightnessctl} -r";
          }
          # Lock screen after five minutes of idleness
          {
            timeout = 300;
            command = "loginctl lock-session";
          }
          # Turn off screen after fifteen minutes of idleness
          {
            timeout = 900;
            # On trigger, disable monitors
            command = "swaymsg 'output * dpms off'";
            # On resumption, enable monitors
            resumeCommand = "swaymsg 'output * dpms on'";
          }
          # Suspend/hibernate after thirty minutes of idleness.
          {
            timeout = 1800;
            command = "systemctl suspend";
          }
        ];
      };
    };
  };
}
