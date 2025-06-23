{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (config.robot-disco.laptop) bluetoothID;
  cfg = config.robot-disco.wayland.hyprland;

  cliphist = "${config.services.cliphist.package}/bin/cliphist";
  # Standard clipboard tooling
  wl-copy = "${pkgs.wl-clipboard}/bin/wl-copy";
  # wl-paste = "${pkgs.wl-clipboard}/bin/wl-paste";

  # Wireplumber for volume control
  wpctl = "${pkgs.wireplumber}/bin/wpctl";

  # Application Launcher
  wofi = "${pkgs.wofi}/bin/wofi";
  # File Manager
  thunar = "${pkgs.xfce.thunar}/bin/thunar";

  # Terminal
  kitty = "${pkgs.kitty}/bin/kitty";

  # Status Bar
  waybar = "${pkgs.waybar}/bin/waybar";

  # Brightness control
  brightnessctl = "${pkgs.brightnessctl}/bin/brightnessctl";

  # rfkill
  rfkill = "${pkgs.util-linux}/bin/rfkill";

  # Discord
  discord = "${pkgs.webcord}/bin/webcord";
in
{
  options.robot-disco.wayland.hyprland.enable = lib.mkEnableOption "Hyprland graphical environment";
  config = lib.mkIf cfg.enable {
    # Enable other wayland environmental features
    robot-disco.wayland.wallpaper.enable = true;
    robot-disco.wayland.lock-screen.enable = true;

    services = {
      # Clipboard history
      cliphist.enable = true;
      # Notification popup messages
      mako.enable = true;
    };

    wayland.windowManager.hyprland = {
      inherit (cfg) enable;

      settings = {
        "$terminal" = kitty;
        "$fileManager" = thunar;
        "$menu" = "${wofi} --show drun";

        "$mainMod" = "SUPER";

        # Keyboard input settings
        input = {
          kb_layout = "us";
          kb_options = "ctrl:nocaps";
        };

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

          # Clipboard management via cliphist
          "$mainMod, V, exec, ${cliphist} list | ${wofi} -S dmenu | ${cliphist} decode | ${wl-copy}"
          # Lock Screen
          "$mainMod, L, exec, ${pkgs.systemd}/bin/loginctl lock-session"
        ];

        # Bindings that will repeat when held, and work when locked
        bindel = [
          # Volume control
          ", XF86AudioMute, exec, ${wpctl} set-mute @DEFAULT_AUDIO_SINK@ toggle"
          ", XF86AudioLowerVolume, exec, ${wpctl} set-volume @DEFAULT_AUDIO_SINK@ 5%-"
          ", XF86AudioRaiseVolume, exec, ${wpctl} set-volume @DEFAULT_AUDIO_SINK@ 5%+"
          # Brightness control
          ", XF86MonBrightnessDown, exec, ${brightnessctl} set 5%-"
          ", XF86MonBrightnessUp, exec, ${brightnessctl} set 5%+"
        ];

        # bindings that worked even when the screen is locked
        bindl = [
          # toggle hardware radio on/off (wifi, bluetooth)
          ", XF86RFKill , exec, ${rfkill} toggle 0 && ${rfkill} toggle ${toString bluetoothID}"
        ];

        # Mouse bindings
        bindm = [
          # Move/resize windows with mainMod + LMB/RMB and dragging
          "$mainMod, mouse:272, movewindow"
          "$mainMod, mouse:273, resizewindow"
        ];

        env = [
          # Use wayland for GTK apps.
          "GDK_BACKEND,wayland"
          # Get Chromium and Electron-based apps to use wayland directly without
          # X.
          "NIXOS_OZONE_WL,1"
          # QT apps need to be configured to use wayland as their rendering
          # system
          "QT_QPA_PLATFORM,wayland"
          # Java needs to be told when it is using tiling window managers.
          "_JAVA_AWT_WM_NONREPARENTING,1"
        ];

        exec-once = [
          # Launch a status bar
          "${waybar}"

          # Launch immediate applications
          "[workspace 2 silent] ${pkgs.brave}/bin/brave"
          "[workspace 1 silent] emacsclient -c"
          "[workspace 4 silent] ${pkgs.slack}/bin/slack"
          "[workspace 4 silent] ${discord}"

          # Run Background apps
          "[workspace name:Sunsama silent] ${pkgs.sunsama}/bin/sunsama"
          "[workspace name:UHK silent] ${pkgs.uhk-agent}/bin/uhk-agent"
        ];

        misc.disable_hyprland_logo = true;

        # Window management rules
        windowrulev2 = [
          "opacity 0.93 0.81, class:.*"
        ];
      };
    };
  };
}
