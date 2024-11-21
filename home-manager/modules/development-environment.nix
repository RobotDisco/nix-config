{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.robot-disco.development-environment;

in
{
  options.robot-disco.development-environment = {
    enable = lib.mkEnableOption "Enable nix-centric developer environment";

    signCommits = lib.mkEnableOption "Sign git commits with GPG key";

    gpgKey = lib.mkOption {
      description = "Public key to sign all git commits with.";
      type = lib.types.str;
    };
    fullname = lib.mkOption {
      description = "Full name to put in git commits.";
      type = lib.types.str;
    };
    email = lib.mkOption {
      description = "Email address to put in git commits.";
      type = lib.types.str;
    };
    defaultBranch = lib.mkOption {
      description = "Main branch should be named this when creating a new git repo.";
      default = "main";
      type = lib.types.str;
    };
  };

  config = lib.mkIf cfg.enable (
    lib.mkMerge [
      {
        programs.git = {
          enable = true;
          extraConfig = {
            core = {
              autocrlf = "input";
            };
            hub = {
              protocol = "https";
            };
            init = {
              inherit (cfg) defaultBranch;
            };
            # Configuration for Emacs' Magit Forge package
            # https://magit.vc/manual/forge.html
            github = {
              user = "RobotDisco";
            };
            gitlab."git.internal.tulip.io" = {
              user = "gaelan";
            };
          };
          lfs.enable = true;
          userEmail = cfg.email;
          userName = cfg.fullname;
        };
      }
      (lib.mkIf cfg.signCommits {
        programs.git = {
          signing = {
            signByDefault = true;
            key = cfg.gpgKey;
          };
        };
      })
      {
        programs.jq.enable = true;
        home.packages = [
          # Nix-based development-environment manager
          pkgs.devenv
          # Nix LSP
          pkgs.nil
          # Nix linter
          pkgs.nixfmt-rfc-style
          # Better than grep
          pkgs.ripgrep
        ];
      }
      {
        programs.direnv = {
          enable = true;
          enableZshIntegration = true;
          nix-direnv = {
            enable = true;
          };
        };
      }
    ]
  );
}
