{
  programs.aerospace = {
    enable = true;
    launchd.enable = true;
    userSettings = {
      # "config-version" = 2;
      # Home-manager handles this.
      "start-at-login" = false;

      after-startup-command = [
        "exec-and-forget /etc/profiles/per-user/gaelan/bin/emacsclient -nc"
      ];

      on-focused-monitor-changed = [
        "move-mouse monitor-lazy-center"
      ];

      #persistent-workspaces = [
      #  "1"
      #  "2"
      #  "3"
      #  "4"
      #  "5"
      #  "6"
      #  "7"
      #  "8"
      #  "9"
      #];

      mode = {
        main.binding = {
          "alt-enter" = "exec-and-forget /etc/profiles/per-user/gaelan/bin/emacsclient -nc";

          # Switch layouts
          "alt-slash" = "layout tiles horizontal vertical";
          "alt-comma" = "layout accordion horizontal vertical";

          # Switch windows
          "alt-h" = "focus left";
          "alt-j" = "focus down";
          "alt-k" = "focus up";
          "alt-l" = "focus right";

          # Move windows around
          "alt-shift-h" = "move left";
          "alt-shift-j" = "move down";
          "alt-shift-k" = "move up";
          "alt-shift-l" = "move right";

          # Resize window
          "alt-minus" = "resize smart -50";
          "alt-equal" = "resize smart +50";

          # Switch workplaces
          "alt-1" = "workspace 1";
          "alt-2" = "workspace 2";
          "alt-3" = "workspace 3";
          "alt-4" = "workspace 4";
          "alt-5" = "workspace 5";
          "alt-6" = "workspace 6";
          "alt-7" = "workspace 7";
          "alt-8" = "workspace 8";
          "alt-9" = "workspace 9";

          # Move window to workspaces
          "alt-shift-1" = "move-node-to-workspace 1";
          "alt-shift-2" = "move-node-to-workspace 2";
          "alt-shift-3" = "move-node-to-workspace 3";
          "alt-shift-4" = "move-node-to-workspace 4";
          "alt-shift-5" = "move-node-to-workspace 5";
          "alt-shift-6" = "move-node-to-workspace 6";
          "alt-shift-7" = "move-node-to-workspace 7";
          "alt-shift-8" = "move-node-to-workspace 8";
          "alt-shift-9" = "move-node-to-workspace 9";

          # Switch between current and last workspace
          "alt-tab" = "workspace-back-and-forth";
          # Move workspace to next monitor (wrapping to first if on last monitor)
          "alt-shift-tab" = "move-workspace-to-monitor --wrap-around next";

          "alt-shift-semicolon" = "mode layout";

          "alt-shift-q" = "close";
        };
        layout.binding = {
          # A convenient and safe way to quit layout mode while also providing a
          # way to reload configuration.
          esc = [
            "reload-config"
            "mode main"
          ];
          r = [
            "flatten-workspace-tree"
            "mode main"
          ];
          f = [
            "layout floating tiling"
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
}
