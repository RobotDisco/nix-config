{
  config,
  lib,
  pkgs,
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

        # home-manager's default `bars` value hardcodes trayOutput =
        # "primary", but sway/Wayland has no concept of a primary
        # output, so the tray never renders:
        # https://github.com/nix-community/home-manager/blob/3b0e6bbd65869af1beadf5963a99befc179d209f/modules/services/window-managers/i3-sway/lib/options.nix#L814
        #
        # Nix's module system can't merge into one field of a list
        # item, so overriding `bars` at all discards that whole default
        # record — this reproduces it verbatim, with only trayOutput
        # changed. trayOutput is the only field in that record with
        # sway-specific branching anywhere in options.nix (see `isI3`
        # a few lines above L814); every other field here is either a
        # generic enum/color/font value or already computed correctly
        # for sway by home-manager itself, so none of them need the
        # same treatment.
        bars = [
          {
            mode = "dock";
            hiddenState = "hide";
            position = "bottom";
            workspaceButtons = true;
            workspaceNumbers = true;
            statusCommand = "${pkgs.i3status}/bin/i3status";
            fonts = {
              names = [ "monospace" ];
              size = 8.0;
            };
            trayOutput = "*";
            colors = {
              background = "#000000";
              statusline = "#ffffff";
              separator = "#666666";
              focusedWorkspace = {
                border = "#4c7899";
                background = "#285577";
                text = "#ffffff";
              };
              activeWorkspace = {
                border = "#333333";
                background = "#5f676a";
                text = "#ffffff";
              };
              inactiveWorkspace = {
                border = "#333333";
                background = "#222222";
                text = "#888888";
              };
              urgentWorkspace = {
                border = "#2f343a";
                background = "#900000";
                text = "#ffffff";
              };
              bindingMode = {
                border = "#2f343a";
                background = "#900000";
                text = "#ffffff";
              };
            };
          }
        ];

        assigns = {
          "focus" = [ { app_id = "emacs"; } ];
          "web" = [ { app_id = "vivaldi-stable"; } ];
          "comms" = [
            { app_id = "Slack"; }
            { app_id = "signal"; }
            { class = "discord"; }
            { app_id = "wasistlos"; }
          ];
          "gaming" = [ { class = "steam"; } ];
        };

        startup = [
          # Initialize comms workspace with tabbed layout at startup.
          {
            command = "swaymsg 'workspace comms; layout tabbed; workspace focus'";
          }
        ];

        keybindings = lib.mkOptionDefault {
          XF86AudioMute = "exec wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle";
          XF86AudioLowerVolume = "exec wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-";
          XF86AudioRaiseVolume = "exec wpctl set-volume -l 1.0 @DEFAULT_AUDIO_SINK@ 5%+";
          XF86MonBrightnessDown = "exec brightnessctl set 5%-";
          XF86MonBrightnessUp = "exec brightnessctl set 5%+";
          XF86RFKill = "exec rfkill toggle 0; rfkill toggle all";

          # Focus monitor
          "Mod4+comma" = "focus output prev";
          "Mod4+period" = "focus output next";

          # Named workspace switching
          "Mod4+1" = "workspace focus";
          "Mod4+2" = "workspace web";
          "Mod4+3" = "workspace comms";
          "Mod4+4" = "workspace gaming";
          "Mod4+Shift+1" = "move container to workspace focus";
          "Mod4+Shift+2" = "move container to workspace web";
          "Mod4+Shift+3" = "move container to workspace comms";
          "Mod4+Shift+4" = "move container to workspace gaming";

          # Move workspace to monitor
          "Mod4+Shift+comma" = "move workspace to output prev";
          "Mod4+Shift+period" = "move workspace to output next";

          # Jump back to last workspace
          "Mod4+Tab" = "workspace back_and_forth";

          # Scratchpad
          "Mod4+grave" = "scratchpad show";
          "Mod4+Shift+grave" = "move container to scratchpad";

          # Lock screen
          "Mod4+ctrl+l" = "exec loginctl lock-session";

          # Screenshots (p for picture/print)
          "Mod4+p" = "exec grim ~/Desktop/$(date +%Y%m%d-%H%M%S).png";
          "Mod4+Shift+p" = "exec grim -g \"$(slurp)\" ~/Desktop/$(date +%Y%m%d-%H%M%S).png";

          # Disable unused default numbered workspace bindings
          "Mod4+5" = null;
          "Mod4+6" = null;
          "Mod4+7" = null;
          "Mod4+8" = null;
          "Mod4+9" = null;
          "Mod4+0" = null;
          "Mod4+Shift+5" = null;
          "Mod4+Shift+6" = null;
          "Mod4+Shift+7" = null;
          "Mod4+Shift+8" = null;
          "Mod4+Shift+9" = null;
          "Mod4+Shift+0" = null;
        };
      };
    };
  };
}
