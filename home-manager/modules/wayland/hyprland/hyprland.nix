{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.robot-disco.wayland.hyprland;
in
{
  options.robot-disco.wayland.hyprland.enable = lib.mkEnableOption "Hyprland graphical environment";
  config = lib.mkIf cfg.enable {
    robot-disco.wayland.enable = true;

    programs.waybar.enable = true;

    wayland.windowManager.hyprland = {
      enable = true;
      package = null;
      portalPackage = null;

      configType = "hyprlang";

      settings = {
        "$mod" = "SUPER";

        input = {
          kb_layout = "us";
          kb_options = "ctrl:nocaps";
        };

        # Vanilla bindings
        bind = [
          "$mod, Return, exec, emacsclient -c"
          "$mod, D, exec, dmenu_run"
          "$mod SHIFT, K, killactive"
          "$mod SHIFT, C, exit"

          "$mod, B, movefocus, l"
          "$mod, F, movefocus, r"
          "$mod, P, movefocus, u"
          "$mod, N, movefocus, d"

          "$mod, 1, workspace, 1"
          "$mod, 2, workspace, 2"
          "$mod, 3, workspace, 3"
          "$mod, 4, workspace, 4"
          "$mod, 5, workspace, 5"
          "$mod, 6, workspace, 6"
          "$mod, 7, workspace, 7"
          "$mod, 8, workspace, 8"
          "$mod, 9, workspace, 9"
          "$mod, 0, workspace, 0"

          "$mod SHIFT, 1, movetoworkspace, 1"
          "$mod SHIFT, 2, movetoworkspace, 2"
          "$mod SHIFT, 3, movetoworkspace, 3"
          "$mod SHIFT, 4, movetoworkspace, 4"
          "$mod SHIFT, 5, movetoworkspace, 5"
          "$mod SHIFT, 6, movetoworkspace, 6"
          "$mod SHIFT, 7, movetoworkspace, 7"
          "$mod SHIFT, 8, movetoworkspace, 8"
          "$mod SHIFT, 9, movetoworkspace, 9"
          "$mod SHIFT, 0, movetoworkspace, 10"

          # Focus monitor
          "$mod, comma, focusmonitor, -1"
          "$mod, period, focusmonitor, +1"

          # Move window to monitor
          "$mod SHIFT, comma, movewindow, mon:-1"
          "$mod SHIFT, period, movewindow, mon:+1"
        ];
        # Work when locked, repeat when held
        bindel = [
          # Brightness control
          ", XF86MonBrightnessDown, exec, brightnessctl set 5%-"
          ", XF86MonBrightnessUp, exec, brightnessctl set 5%+"
          # Volume control
          ", XF86AudioMute, exec, wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle"
          ", XF86AudioLowerVolume, exec, wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-"
          ", XF86AudioRaiseVolume, exec, wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%+"
        ];
        # Work when screen is locked
        bindl = [
          # toggle hardware radio on/off (wifi, bluetooth)
          ", XF86RFKill , exec, rfkill toggle all"
        ];

        exec-once = [
          "${pkgs.uwsm}/bin/uwsm app -- waybar"
        ];

        misc.disable_hyprland_logo = true;
      };

      # Because we use UWSM, disable builtin systemd support.
      systemd.enable = false;
    };
  };
}
