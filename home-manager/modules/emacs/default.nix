{
  config,
  lib,
  pkgs,
  robotdisco-secrets,
  ...
}:

let
  inherit (lib) mkEnableOption mkOption types;

  cfg = config.robot-disco.emacs;

  # Do we use the server or non-server emacs for editing?
  emacsBin =
    if cfg.enableServer then "${cfg.package}/bin/emacsclient" else "${cfg.package}/bin/emacs";

  # Populate configuration for files we need home-manager to lay down.
  mkEmacsConfigFiles =
    path:
    lib.foldl' (acc: file: acc // { "emacs/${file}".source = "${path}/${file}"; }) { } (
      lib.attrNames (builtins.readDir path)
    );
in
{
  options.robot-disco.emacs = {
    enable = mkEnableOption "enable gaelan's custom emacs configuration.";

    package = mkOption {
      type = types.package;
      default = pkgs.gaelan-emacs;
      defaultText = lib.literalExample "pkgs.gaelan-emacs";
      description = "The Emacs derivation to use.";
    };

    configPackage = lib.mkOption {
      type = types.package;
      default = pkgs.gaelan-emacs-config;
      defaultText = lib.literalExample "pkgs.gaelan-emacs-config";
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

    emacsBin = mkOption {
      type = types.string;
      default = emacsBin;
      readOnly = true;
    };
  };

  config = lib.mkIf cfg.enable (
    lib.mkMerge [
      # Hack to make launching emacs less irritating.
      (lib.mkIf pkgs.stdenv.isDarwin {
        programs.zsh.shellAliases.emacs = "${cfg.package}/Applications/Emacs.app/Contents/MacOS/Emacs";
      })
      {
        # Temporary place to install packages I know we need with our emacs
        # config.
        # TODO this probably should be coupled with the emacs config somehow.
        home.packages = with pkgs; [
          # Dictionary support
          (aspellWithDicts (
            dicts: with dicts; [
              en
              en-science
              en-computers
            ]
          ))
          # org-roam graph support
          graphviz
          # Graphics support
          imagemagick
          # LaTeX support
          texlive.combined.scheme-full
          # ePub support
          unzip
        ];
      }
      {
        services.emacs = {
          enable = cfg.enableServer;
          inherit (cfg) package;
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

        # Install our emacs package, as well as the emacs config package if XDG config mangement was enabled.
        home.packages = [
          cfg.package
        ]
        ++ lib.optionals cfg.enableUserDirectory cfg.configPackage.buildInputs;

      }
      (lib.mkIf cfg.enableUserDirectory {
        age.secrets = {
          emacs-authinfo.rekeyFile = "${robotdisco-secrets}/emacs-authinfo.age";
          emacs-xoauth2-el.rekeyFile = "${robotdisco-secrets}/emacs-xoauth2-el.age";
        };

        home = {
          activation.linkEmacsAuthinfo = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
            run ln -sf $VERBOSE_ARG "${config.age.secrets.emacs-authinfo.path}" "${config.home.homeDirectory}/.authinfo";
          '';
          activation.linkEmacsXoauth2El = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
            run ln -sf $VERBOSE_ARG "${config.age.secrets.emacs-xoauth2-el.path}" "${config.xdg.configHome}/emacs/xoauth2.el";
          '';
        };

        xdg = {
          enable = true;
          configFile = mkEmacsConfigFiles cfg.configPackage;
        };
      })
      (lib.mkIf cfg.defaultEditor { home.sessionVariables.EDITOR = emacsBin; })
      (lib.mkIf cfg.enableGitDiff { programs.git.extraConfig.diff.tool = "ediff"; })
      {
        home.shellAliases = {
          erecovers = "find ~/Documents/brain -name '#*#' -print";
        };
      }
    ]
  );
}
