{ config, lib, pkgs, ... }:

let cfg = config.robot-disco.development-environment;

in {
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

  config = lib.mkIf cfg.enable (lib.mkMerge [
    {
      programs.git = {
        enable = true;
        extraConfig = {
          core = { autocrlf = "input"; };
          hub = { protocol = "https"; };
          init = { defaultBranch = cfg.defaultBranch; };
        };

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
        # Nix LSP
        pkgs.nil
        # Better than grep
        pkgs.ripgrep
      ];
    }
    {
      programs.direnv = {
        enable = true;
        enableZshIntegration = true;
        nix-direnv = { enable = true; };
      };
    }
  ]);
}
