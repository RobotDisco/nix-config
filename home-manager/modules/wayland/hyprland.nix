{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (config.robot-disco.laptop) bluetoothID;
  cfg = config.robot-disco.wayland.hyprland;

  # Clipboard managers I'm evaluating
  # clipse = "${pkgs.clipse}/bin/clipse";
  cliphist = "${pkgs.cliphist}/bin/cliphist";
  # Standard clipboard tooling
  wl-copy = "${pkgs.wl-clipboard}/bin/wl-copy";
  wl-paste = "${pkgs.wl-clipboard}/bin/wl-paste";

  # Wireplumber for volume control
  wpctl = "${pkgs.wireplumber}/bin/wpctl";

  # Application Launcher
  wofi = "${pkgs.wofi}/bin/wofi";

  # Notifications
  mako = "${pkgs.mako}/bin/mako";

  # File Manager
  thunar = "${pkgs.xfce.thunar}/bin/thunar";

  # Terminal
  kitty = "${pkgs.kitty}/bin/kitty";

  # Status Bar
  waybar = "${pkgs.waybar}/bin/waybar";

  # Emacs
  # emacs = "${pkgs.emacs}/bin/emacs";

  # Brightness control
  brightnessctl = "${pkgs.brightnessctl}/bin/brightnessctl";

  # rfkill
  rfkill = "${pkgs.util-linux}/bin/rfkill";
in
{
  options.robot-disco.wayland.hyprland.enable = lib.mkEnableOption "Hyprland graphical environment";
  config = {
    wayland.windowManager.hyprland = {
      inherit (cfg) enable;

      settings = {
        "$lapMon" = "eDP-1";
        "$lapMonScale" = "1.566667";

        "$terminal" = kitty;
        "$fileManager" = thunar;
        "$menu" = "${wofi} --show drun";

        "$mainMod" = "SUPER";

        # Keyboard input settings
        input = {
          kb_layout = "us";
          kb_options = "ctrl:nocaps";
        };

        monitor = [
          # Framework 1.5x builtin monitor
          "$lapMon, preferred, auto, $lapMonScale"
          "desc:Dell Inc. DELL U2412M M2GCR1CS0T1L, preferred, auto, 1"
          "desc:Dell Inc. DELL U2412M HT5N364F0GSS, preferred, auto, 1, transform, 3"
          # Render by default on any other monitor that's connected.
          ",preferred,auto,auto"
        ];

        # vanilla bindings
        bind = [
          "$mainMod, Return, exec, $terminal"
          "$mainMod SHIFT, Q, killactive"
          "$mainMod SHIFT, E, exit"
          "$mainMod SHIFT, F, exec, $fileManager"
          "$mainMod, F, togglefloating,"
          "$mainMod, D, exec, $menu"
          # Dwindle
          "$mainMod, P, pseudo,"
          "$mainMod, S, togglesplit,"

          # Move focus with mainMod + arrow keys
          "$mainMod, H, movefocus, l"
          "$mainMod, L, movefocus, r"
          "$mainMod, K, movefocus, u"
          "$mainMod, J, movefocus, d"

          # Switch workspaces with mainMod + [0-9]
          "$mainMod, 1, workspace, 1"
          "$mainMod, 2, workspace, 2"
          "$mainMod, 3, workspace, 3"
          "$mainMod, 4, workspace, 4"
          "$mainMod, 5, workspace, 5"
          "$mainMod, 6, workspace, 6"
          "$mainMod, 7, workspace, 7"
          "$mainMod, 8, workspace, 8"
          "$mainMod, 9, workspace, 9"
          "$mainMod, 0, workspace, 10"

          # Move active window to a workspace with mainMod + SHIFT + [0-9]
          "$mainMod SHIFT, 1, movetoworkspace, 1"
          "$mainMod SHIFT, 2, movetoworkspace, 2"
          "$mainMod SHIFT, 3, movetoworkspace, 3"
          "$mainMod SHIFT, 4, movetoworkspace, 4"
          "$mainMod SHIFT, 5, movetoworkspace, 5"
          "$mainMod SHIFT, 6, movetoworkspace, 6"
          "$mainMod SHIFT, 7, movetoworkspace, 7"
          "$mainMod SHIFT, 8, movetoworkspace, 8"
          "$mainMod SHIFT, 9, movetoworkspace, 9"
          "$mainMod SHIFT, 0, movetoworkspace, 10"

          # Example special workspace (scratchpad)
          "$mainMod, M, togglespecialworkspace, magic"
          "$mainMod SHIFT, M, movetoworkspace, special:magic"

          # Scroll through existing workspaces with mainMod + scroll
          "$mainMod, mouse_down, workspace, e+1"
          "$mainMod, mouse_up, workspace, e-1"

          # Clipboard management
          # cliphist
          "$mainMod, V, exec, ${cliphist} list | ${wofi} -S dmenu | ${cliphist} decode | ${wl-copy}"
          # clipse
          # ''$mainMod, V, exec, $terminal --class clipse -e 'clipse'''
        ];

        # Bindings that will repeat when held, and work when locked
        bindel = [
          # Volume control
          ", XF86AudioMute, exec, ${wpctl} set-mode @DEFAULT_AUDIO_SINK@ toggle"
          ", XF86AudioLowerVolume, exec, ${wpctl} set-volume @DEFAULT_AUDIO_SINK@ 5%-"
          ", XF86AudioRaiseVolume, exec, ${wpctl} set-volume @DEFAULT_AUDIO_SINK@ 5%+"
          # Brightness control
          ", XF86MonBrightnessDown, exec, ${brightnessctl} set 5%-"
          ", XF86MonBrightnessUp, exec, ${brightnessctl} set 5%+"
        ];

        # bindings that worked even when the screen is locked
        bindl = [
          # trigger when laptop lid is closed
          ", switch:on:Lid Switch, exec, hyprctl keyword monitor $lapMon, disable"
          # trigger when laptop lid is closed
          ", switch:off:Lid Switch, exec, hyprctl keyword monitor $lapMon, preferred, auto-below, $lapMonScale"
          # toggle hardware radio on/off (wifi, bluetooth)
          ", XF86RFKill , exec, ${rfkill} toggle 0 && ${rfkill} toggle ${toString bluetoothID}"
        ];

        # Mouse bindings
        bindm = [
          # Move/resize windows with mainMod + LMB/RMB and dragging
          "$mainMod, mouse:272, movewindow"
          "$mainMod, mouse:273, resizewindow"
        ];

        exec-once = lib.concatStringsSep " " [
          # Try out cliphist
          "${wl-paste} --watch ${cliphist} store &"
          # Try out clipse
          # "${clipse} -listen & "

          # Notification Messages
          "${mako} &"

          # Launch a status bar
          "${waybar} &"

          # We almost always want to run Emacs from the get-go
          "emacsclient -c"
        ];

        # Window Management triggers
        windowrulev2 = [
          # Ensure you have a floating class for clipse"
          "float,class:(clipse)"
          # Set the window size of a clipse buffer
          "size 622 652,class:(clipse)"
          # You'll probably like this (why?)
          "suppressevent maximize, class:.*"
        ];
      };
    };
  };
}
