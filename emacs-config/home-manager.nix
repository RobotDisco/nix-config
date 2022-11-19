{ config, lib, pkgs, ... }:

let
  inherit (lib) mkEnableOption mkOption types;
  
  cfg = config.robot-disco.emacs;

  # Do we use the server or non-server emacs for editing?
  emacsBin = if cfg.enableServer then "${cfg.package}/bin/emacsclient" else "${cfg.package}/bin/emacs";

  # Populate configuration for files we need home-manager to lay down.
  mkEmacsConfigFiles = path:
    lib.foldl'
    (acc: file: acc // { "emacs/${file}".source = "${path}/${file}"; }) { }
    (lib.attrNames (builtins.readDir path));
in {
  options.robot-disco.emacs = {
    enable = mkEnableOption "enable gaelan's custom emacs configuration.";

    package = mkOption {
      type = types.package;
      default = pkgs.emacsEnv;
      defaultText = lib.literalExample "pkgs.emacsEnv";
      description = "The Emacs derivation to use.";
    };

    configPackage = lib.mkOption {
      type = types.package;
      default = pkgs.emacsConfig;
      defaultText = lib.literalExample "pkgs.emacsConfig";
      description = "The Emacs configuration derivation to use.";
    };

    enableUserDirectory = mkOption {
      type = types.bool;
      default = true;
      description = "Install user's Emacs directory files.";
    };

    enableGitDiff = mkOption {
      type = types.bool;
      default = true;
      description = "Enable ediff as default git diff tool.";
    };

    enableServer = mkOption {
      type = types.bool;
      default = pkgs.stdenv.isLinux;
      description = "Whether to enable user Emacs server.";
    };

    defaultEditor = mkOption {
      type = types.bool;
      default = true;
      description = "Whether to use Emacs as default editor.";
    };

    enableExwm = mkEnableOption {
      type = types.bool;
      default = false;
      description = "Whether to enable EXWM as a window manager. Make sure your emacs package includes EXWM.";
    };
  };

  config = lib.mkIf cfg.enable (lib.mkMerge [
    {
      services.emacs = {
        enable = cfg.enableServer;
        package = cfg.package;
        socketActivation.enable = true;
      };

      programs.git.extraConfig = {
        difftool.diff.cmd = ''
          ${emacsBin} --eval '(ediff-files "'$LOCAL'" "'$REMOTE'")'
        '';

        mergetool.ediff.cmd = ''
          ${emacsBin} --eval '(ediff-merge-files-with-ancestor "'$LOCAL'" "'$REMOTE'" '"$BASE'" nil "'$MERGED'")'
        '';
      };

      # Install any additional package specified by the emacs-config derivation, as well as our specified emacs package
      home.packages = [ cfg.package ]
        ++ lib.optionals cfg.enableUserDirectory cfg.configPackage.buildInputs;

    }
    (lib.mkIf cfg.enableUserDirectory {
      xdg = {
        enable = true;
        configFile = mkEmacsConfigFiles cfg.configPackage;
      };
    })
    (lib.mkIf cfg.defaultEditor { home.sessionVariables.EDITOR = emacsBin; })
    (lib.mkIf cfg.enableGitDiff {
      programs.git.extraConfig.diff.tool = "ediff";
    })
    (lib.mkIf cfg.enableExwm {
      # Enable exwm when launching the emacs server
      services.emacs.extraOptions = [ "-f" "exwm-enable" ];

      # Enable polybar as my system tray / status bar
      services.polybar = {
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

            label-active-font=1;

            modules-left = "date xworkspaces";
            modules-center = "xwindow";
            modules-right= "pulseaudio wlan battery";

            separator = " | ";

            tray.position = "right";
          };
          "module/battery" = {
            type = "internal/battery";
            time.format = "%H:%M";
            battery = "BAT1";
            adapter = "ACAD";
          };
          "module/date" = {
            type = "internal/date";
            date = "T: %H:%M";
            date-alt ="%Y/%m/%d %H:%M:%S";

            label = "%date%";
          };
          "module/wlan" = {
            type = "internal/network";
            interface-type = "wireless";
            format-connected = "<label-connected>";
            format-disconnected = "<label-disconnected>";
            label-disconnected = "W: %ifname%";
            label-connected = "W: %ifname% %essid%";
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

            label-active = "[%name%]";
          };
        };
      };
      # Polybar isn't run graphically by default for some reason
      systemd.user.services.polybar = {
        Install.WantedBy = [ "graphical-session.target" ];
      };

      # We're leveraging .xsession support to load our window manager, as
      # services.xserver.windowManager.exwm doesn't suffice for my needs.
      xsession.enable = if pkgs.stdenv.isLinux then true else false;
      xsession.windowManager.command = if cfg.enableServer then
        "${emacsBin} -c"
      else
        "${emacsBin} -f exwm-enable";

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
      ];
    })
  ]);
}
