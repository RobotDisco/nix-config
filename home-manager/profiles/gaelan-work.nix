{
  config,
  pkgs,
  ...
}:

let
  username = "gaelan";
  emacsclient = "${config.home.profileDirectory}/bin/emacsclient";
in
{
  config = {
    robot-disco = {
      claude-code = {
        enable = true;

        settings = {
          enabledPlugins = {
            "atlassian@claude-plugins-official" = true;
            "php-lsp@claude-plugins-official" = true;
            "security-guidance@claude-plugins-official" = true;
            "slack@claude-plugins-official" = true;
            "incident-investigator@tulip-agent-context" = true;
          };
          marketplaces = {
            tulip = pkgs.fetchFromGitLab {
              domain = "git.internal.tulip.io";
              owner = "developer-tools/ai";
              private = true;
              repo = "agent-context";
              rev = "10f72ec4f358431696963d10612b4b3921ea36fa";
              sha256 = "C+Wt1Jl+exCpAyxL4gHJ9ejfnGOStB7PwD5EDKkKh7s=";
            };
          };
          permissions = {
            deny = [
              "mcp__GitLab__manage_pipeline"
            ];
            ask = [
              "mcp__GitLab__create_issue"
              "mcp__GitLab__create_workitem_note"
            ];
          };
        };

        mcpServers = {
          GitLab = {
            type = "http";
            url = "https://git.internal.tulip.io/api/v4/mcp";
          };
        };
      };

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

      # I sometimes want to run claude using my personal account. To do this,
      # I switch what claude considers the home directory.
      shellAliases.claude-personal = "CLAUDE_CONFIG_DIR=~/.claude-personal claude";
    };

    programs = {
      zsh.enable = true;

      aerospace = {
        enable = true;
        launchd.enable = true;
        settings = {
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

          on-window-detected = [
            {
              "if".app-name-regex-substring = "Emacs";
              run = "move-node-to-workspace focus";
            }
            {
              "if".app-name-regex-substring = "Vivaldi";
              run = "move-node-to-workspace web";
            }
            {
              "if".app-name-regex-substring = "Slack";
              run = "move-node-to-workspace comms";
            }
            {
              "if".app-name-regex-substring = "Tidal";
              run = "move-node-to-workspace fun";
            }
            {
              "if".app-name-regex-substring = "Discord";
              run = "move-node-to-workspace fun";
            }
          ];

          mode = {
            main.binding = {
              # Workspace switching
              "alt-1" = "workspace focus";
              "alt-2" = "workspace web";
              "alt-3" = "workspace comms";
              "alt-4" = "workspace fun";

              # Move windows to workspaces
              "alt-shift-1" = "move-node-to-workspace focus";
              "alt-shift-2" = "move-node-to-workspace web";
              "alt-shift-3" = "move-node-to-workspace comms";
              "alt-shift-4" = "move-node-to-workspace fun";
            }
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

              # Move workspace to monitor (sway: $mod+Shift+comma / $mod+Shift+period)
              "alt-shift-comma" = "move-workspace-to-monitor --wrap-around prev";
              "alt-shift-period" = "move-workspace-to-monitor --wrap-around next";

              # Screenshots (sway: $mod+p / $mod+Shift+p)
              "alt-p" = "exec-and-forget screencapture ~/Desktop/$(date +%Y%m%d-%H%M%S).png";
              "alt-shift-p" = "exec-and-forget screencapture -i ~/Desktop/$(date +%Y%m%d-%H%M%S).png";

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
