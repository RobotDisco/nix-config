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

      # We're leveraging .xsession support to load our window manager, as
      # services.xserver.windowManager.exwm doesn't suffice for my needs.
      xsession.enable = if pkgs.stdenv.isLinux then true else false;
      xsession.windowManager.command = if cfg.enableServer then
        "${emacsBin} -c"
      else
        "${emacsBin} -f exwm-enable";
    })
  ]);
}
