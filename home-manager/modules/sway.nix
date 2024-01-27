{ config, lib, pkgs, ... }:

let
  cfg = config.robot-disco.sway;
in

{
  options.robot-disco.sway.enable = lib.mkEnableOption "Enable Sway WM";

  config = lib.mkIf cfg.enable {
    wayland.windowManager.sway = {
      enable = true;

      # systemd.xdgAutostart = true;

      extraSessionCommands = ''
        # Fix for some Java AWT applications (e.g. Android Studio),
        # use this if they aren't displayed properly:
        export _JAVA_AWT_WM_NONREPARENTING=1
      '';
    
      config = {
        modifier = "Mod4";
#        left = "b";
#        down = "n";
#        up = "p";
#        right = "f";
#        terminal = "${pkgs.emacs}/bin/emacsclient -c";
#        menu = "${pkgs.dmenu}/bin/dmenu_run | ${pkgs.dmenu}/bin/dmenu | ${pkgs.fileutils}/bin/xargs swaymsg exec --";
        input."type:keyboard".xkb_options = "ctrl:nocaps";
        output."eDP-1" = {
          resolution = "2256x1504 scale 1.5";
        };
      };
    };
  };
}
