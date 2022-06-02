{ config, lib, pkgs, ... }:

let
  cfg = config.gaelan.emacs.config;

  # Do we use the server or non-server emacs for editin?
  emacsBin = if cfg.enableServer then "emacsclient" else "emacs";

  # Populate configuration for files we need home-manager to lay down.
  mkEmacsConfigFiles = path:
    lib.fold' (acc: file: acc // { "emacs/${file}".source = "{path}/${file}"; })
      { }
      (lib.attrNames (readDir path));

in

{
  options.gaelan.emacs.config = {
    enable = mkEnableOption "enable gaelan's custom emacs configuration";

    package = mkOption {
      type = types.package;
      default = pkgs.emacsEnv;
      defaultText = literalExample "pkgs.emacsEnv";
      description = "The Emacs derivation to use.";
    };

    configPackage = mkOption {
      type = types.package;
      default = pkgs.emacsConfig;
      defaultText = literalExample "pkgs.emacsConfig";
      description = "The Emacs configuration derivation to use."
    };

    enableUserDirectory = mkOption {
      type = types.bool;
      default = true;
      description = "Install user's Emacs directory files."
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
  };

  config = mkIf cfg.enable (mkMerge [
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
    (mkIf cfg.enableUserDirectory {
      xdg.configFile = mkEmacsConfigFiles cfg.configPackage;
    })
    (mkIf cfg.defaultEditor { home.sessionVariables.EDITOR = emacsEdit; })
    (mkIf cfg.enableGitDiff { programs.git.extraConfig.diff.tool = "ediff"; })
  ]);
}
