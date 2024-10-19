{
  config,
  lib,
  pkgs,
}:

let
  cfgEmacs = config.robot-disco.emacs;
  cfg = cfgEmacs.exwm;

  inherit (cfgEmacs) emacsBin;
in
{
  options.robot-disco.emacs.exwm.enable = lib.mkEnableOption {
    description = "Whether to enable EXWM as a window manager. Make sure your emacs package includes EXWM.";
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = pkgs.stdenv.isLinux;
        message = "EXWM can only be run on Linux";
      }
    ];

    services = {
      blueman-applet.enable = true;
      # Notification widget
      dunst.enable = true;

      # Enable exwm when launching the emacs server
      emacs.extraOptions = [
        "-f"
        "exwm-enable"
      ];

      network-manager-applet.enable = true;

      # Volume controller
      pasystray.enable = true;
      # Graphical Compositor
      picom.enable = true;

      # Enable polybar as my system tray / status bar
      polybar = {
        enable = true;
        script = ''
          #!/usr/bin/env bash

          # Terminate already-existing polybar instances
          # If IPC is enabled for all bars
          ${config.services.polybar.package}/bin/polybar-msg cmd quit
          # Otherwise use killall
          # killall -q polybar

          # Launch a bar
          ${config.services.polybar.package}/bin/polybar & disown
        '';
        settings = {
          "bar/gaelan" = {
            dpi-x = 0;
            dpi-y = 0;

            enable.ipc = true;

            font-0 = "Anonymous Pro:size=8:weight=bold;2";

            label-active-font = 1;

            modules-left = "date xworkspaces";
            modules-center = "xwindow";
            modules-right = "pulseaudio wlan battery";

            separator = " | ";

            tray.position = "right";
          };
          "module/battery" = {
            type = "internal/battery";
            time.format = "B: %H:%M";
            battery = "BAT1";
            adapter = "ACAD";
          };
          "module/date" = {
            type = "internal/date";
            date = "T: %H:%M";
            date-alt = "%Y/%m/%d %H:%M:%S";

            label = "%date%";
          };
          "module/wlan" = {
            type = "internal/network";
            interface-type = "wireless";
            format-connected = "W: <label-connected>";
            format-disconnected = "W: <label-disconnected>";
            label-disconnected = "<disconnected>";
            label-connected = "%essid%";
          };
          "module/pulseaudio" = {
            type = "internal/pulseaudio";
            label-volume = "V: %percentage%%";
            label-muted = "V: muted";
          };
          "module/xwindow" = {
            type = "internal/xwindow";
            label = "%title:0:60:...%";
          };
          "module/xworkspaces" = {
            type = "internal/xworkspaces";

            label-active = " [%name%]";

            pin-workspaces = true;

            enable-click = true;
          };
          "settings" = {
            screenchange-reload = true;
          };
        };
      };
    };
    # Polybar isn't run graphically by default for some reason
    systemd.user.services.polybar = {
      Install.WantedBy = [ "graphical-session.target" ];
    };

    xsession = {
      # We're leveraging .xsession support to load our window manager, as
      # services.xserver.windowManager.exwm doesn't suffice for my needs.
      enable = true;
      windowManager.command =
        if cfgEmacs.enableServer then "${emacsBin} -c" else "${emacsBin} -f exwm-enable";
    };

    # Install external commands used by emacs when in desktop environment mode
    home.packages = with pkgs; [
      # desktop-environment-mode
      alsa-utils
      brightnessctl
      scrot
      i3lock
      upower
      tlp
      playerctl

      # Fonts
      anonymousPro
      camingo-code

      # System widgets
      pavucontrol
    ];
  };
}
