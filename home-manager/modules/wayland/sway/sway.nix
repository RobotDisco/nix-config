{
  config,
  lib,
  ...
}:

let
  cfg = config.robot-disco.wayland.sway;
in

{
  options.robot-disco.wayland.sway = {
    enable = lib.mkEnableOption "Enable Sway WM";
  };

  config = lib.mkIf cfg.enable {
    robot-disco.wayland.enable = true;

    wayland.windowManager.sway = {
      package = null;
      enable = true;

      systemd.enable = true;

      wrapperFeatures.gtk = true;

      config = {
        modifier = "Mod4";
        terminal = "uwsm app -- emacsclient -c";
        input."type:keyboard".xkb_options = "ctrl:nocaps";

        keybindings = lib.mkOptionDefault {
          XF86AudioMute = "exec wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle";
          XF86AudioLowerVolume = "exec wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-";
          XF86AudioRaiseVolume = "exec wpctl set-volume -l 1.0 @DEFAULT_AUDIO_SINK@ 5%+";
          XF86MonBrightnessDown = "exec brightnessctl set 5%-";
          XF86MonBrightnessUp = "exec brightnessctl set 5%+";
          XF86RFKill = "exec rfkill toggle 0; rfkill toggle all";
        };
      };
    };
  };
}
