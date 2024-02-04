{ config, lib, pkgs, ... }:

let
  cfg = config.robot-disco.sway;
in

{
  options.robot-disco.sway.enable = lib.mkEnableOption "Enable Sway WM";

  config = lib.mkIf cfg.enable {
    wayland.windowManager.sway = {
      package = null;
      enable = true;

      # systemd.xdgAutostart = true;

      extraSessionCommands = ''
        # Fix for some Java AWT applications (e.g. Android Studio),
        # use this if they aren't displayed properly:
        export _JAVA_AWT_WM_NONREPARENTING=1
        # Make Chrome/Electron-based applications work in Wayland.
        export NIXOS_OZONE_WL="1"
      '';

      extraConfig = ''
        # Enable or disable laptop display based on lid state
        bindswitch --reload --locked lid:on output eDP-1 disable
        bindswitch --reload --locked lid:off output eDP-1 enable
      '';
    
      config = {
        modifier = "Mod4";
#        left = "b";
#        down = "n";
#        up = "p";
#        right = "f";
#        splitv = "v";
#        splith = "h";
#        terminal = "${pkgs.emacs}/bin/emacsclient -c";
#        menu = "${pkgs.dmenu}/bin/dmenu_run | ${pkgs.dmenu}/bin/dmenu | ${pkgs.fileutils}/bin/xargs swaymsg exec --";
        input."type:keyboard".xkb_options = "ctrl:nocaps";
        output."eDP-1" = {
          resolution = "2256x1504";
          scale = "1.5";
        };
        output."*" = {
          bg = "backgrounds/moosevalley.jpg fill";
        };
        # Wayland, unlike autorandr, doesn't keep port names the
        # same. Have to use monitor identifier.
        output."Dell Inc. DELL U2412M M2GCR1CS0T1L" = {
          resolution = "1920x1200";
          position = "0 0";
          scale = "1";
        };
        output."Dell Inc. DELL U2412M HT5N364F0GSS" = {
          resolution = "1920x1200";
          position = "1920 0";
          transform = "90";
          scale = "1";
        };
        keybindings =
          let
            modifier = config.wayland.windowManager.sway.config.modifier;
            brightnessctl = "${pkgs.brightnessctl}/bin/brightnessctl";
            wpctl = "${pkgs.wireplumber}/bin/wpctl";
            rfkill = "${pkgs.util-linux}/bin/rfkill";            
          in lib.mkOptionDefault {
            XF86AudioMute = "exec ${wpctl} set-mute @DEFAULT_AUDIO_SINK@ toggle";
            XF86AudioLowerVolume = "exec ${wpctl} set-volume @DEFAULT_AUDIO_SINK@ 5%-";
            XF86AudioRaiseVolume = "exec ${wpctl} set-volume -l 1.0 @DEFAULT_AUDIO_SINK@ 5%+";
            XF86MonBrightnessDown = "exec ${brightnessctl} set 5%-";
            XF86MonBrightnessUp = "exec ${brightnessctl} set 5%+";
            XF86RFKill = "exec ${rfkill} toggle 0; ${rfkill} toggle 6";
          };  
      };
    };
  };
}
