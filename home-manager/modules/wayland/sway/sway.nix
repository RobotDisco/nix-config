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

      # extraSessionCommands = ''
      # # Fix for some Java AWT applications (e.g. Android Studio),
      # # use this if they aren't displayed properly:
      #   export _JAVA_AWT_WM_NONREPARENTING=1
      # # Make Chrome/Electron-based applications work in Wayland.
      #   export NIXOS_OZONE_WL="1"
      # '';

      config = {
        modifier = "Mod4";
        #        left = "h";
        #        down = "j";
        #        up = "p";
        #        right = "f";
        #        splitv = "v";
        #        splith = "h";
        #        terminal = "${pkgs.emacs}/bin/emacsclient -c";
        #        menu = "${pkgs.dmenu}/bin/dmenu_run | ${pkgs.dmenu}/bin/dmenu | ${pkgs.fileutils}/bin/xargs swaymsg exec --";
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
