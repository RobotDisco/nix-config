{
  config,
  lib,
  robotdisco-secrets,
  ...
}:

let
  username = "gaelan";
  emacsclient = "${config.home.profileDirectory}/bin/emacsclient";
in
{
  imports = [
    ./claude.nix
  ];

  config = {
    age.rekey.hostPubkey = "${robotdisco-secrets}/users/gaelan-work.pub";

    robot-disco = {
      development-environment = {
        enable = true;

        fullname = "Gaelan D'costa";
        email = "gaelan@tulip.com";
        gpgKey = "0x00729AD1F1840227!";

        signCommits = true;
      };

      gnupg.enable = true;

      tulip.enable = true;
    };

    home = {
      inherit username;
      homeDirectory = "/Users/${username}";
      # The state version is required and should stay at the version you
      # originally installed.
      stateVersion = "22.11";
    };

    programs = {
      zsh.enable = true;

      aerospace = {
        enable = true;
        launchd.enable = true;
        userSettings = {
          # Home-manager sets start-at-login via launchd above.
          "start-at-login" = false;

          after-startup-command = [
            "exec-and-forget ${emacsclient} -nc"
          ];

          on-focused-monitor-changed = [
            "move-mouse monitor-lazy-center"
          ];

          gaps = {
            inner.horizontal = 5;
            inner.vertical = 5;
            outer = {
              left = 5;
              right = 5;
              top = 5;
              bottom = 5;
            };
          };

          mode = {
            main.binding =
              # Workspace switching (sway: $mod+N) and
              # moving windows to workspaces (sway: $mod+Shift+N)
              builtins.listToAttrs (
                builtins.concatMap (
                  n:
                  let
                    s = builtins.toString n;
                  in
                  [
                    {
                      name = "alt-${s}";
                      value = "workspace ${s}";
                    }
                    {
                      name = "alt-shift-${s}";
                      value = "move-node-to-workspace ${s}";
                    }
                  ]
                ) (lib.range 1 9)
              )
              // {
                # Launch emacsclient (sway: $mod+Return = terminal)
                "alt-enter" = "exec-and-forget ${emacsclient} -nc";

                # Close window (sway: $mod+Shift+q)
                "alt-shift-q" = "close";

                # Focus movement (sway: $mod+h/j/k/l)
                "alt-h" = "focus left";
                "alt-j" = "focus down";
                "alt-k" = "focus up";
                "alt-l" = "focus right";

                # Move windows (sway: $mod+Shift+h/j/k/l)
                "alt-shift-h" = "move left";
                "alt-shift-j" = "move down";
                "alt-shift-k" = "move up";
                "alt-shift-l" = "move right";

                # Toggle split direction (sway: $mod+e)
                "alt-e" = "layout tiles horizontal vertical";

                # Explicit split direction (sway: $mod+b / $mod+v)
                "alt-b" = "layout tiles horizontal";
                "alt-v" = "layout tiles vertical";

                # Accordion/stacking layout (sway: $mod+s)
                "alt-s" = "layout accordion horizontal vertical";

                # Fullscreen (sway: $mod+f)
                "alt-f" = "fullscreen";

                # Toggle floating (sway: $mod+Shift+space)
                "alt-shift-space" = "layout floating tiling";

                # App launcher (sway: $mod+d)
                "alt-d" = "exec-and-forget open -a Spotlight";

                # Reload config (sway: $mod+Shift+c)
                "alt-shift-c" = "reload-config";

                # Resize (quick smart resize)
                "alt-minus" = "resize smart -50";
                "alt-equal" = "resize smart +50";

                # Enter resize mode (sway: $mod+r)
                "alt-r" = "mode resize";

                # Switch between current and last workspace
                "alt-tab" = "workspace-back-and-forth";

                # Focus monitor (sway: $mod+comma / $mod+period)
                "alt-comma" = "focus-monitor prev";
                "alt-period" = "focus-monitor next";

                # Move window to monitor (sway: $mod+Shift+comma / $mod+Shift+period)
                "alt-shift-comma" = "move-node-to-monitor prev";
                "alt-shift-period" = "move-node-to-monitor next";

                # Move workspace to next monitor
                "alt-shift-tab" = "move-workspace-to-monitor --wrap-around next";

                # Enter layout mode (for join-with operations)
                "alt-shift-semicolon" = "mode layout";
              };

            resize.binding = {
              # Directional resize (sway resize mode: h/j/k/l)
              # Stays in resize mode until esc, matching sway behaviour.
              h = "resize width -50";
              j = "resize height +50";
              k = "resize height -50";
              l = "resize width +50";
              esc = "mode main";
            };

            layout.binding = {
              # Exit layout mode; also reloads config as a safe fallback.
              esc = [
                "reload-config"
                "mode main"
              ];
              r = [
                "flatten-workspace-tree"
                "mode main"
              ];

              "alt-shift-h" = [
                "join-with left"
                "mode main"
              ];
              "alt-shift-j" = [
                "join-with down"
                "mode main"
              ];
              "alt-shift-k" = [
                "join-with up"
                "mode main"
              ];
              "alt-shift-l" = [
                "join-with right"
                "mode main"
              ];
            };
          };
        };
      };
    };
  };
}
