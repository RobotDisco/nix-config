{ config, lib, pkgs }:

let cfg = config.robot-disco.development-environment;

in {
  options.robot-disco.development-environment = {
    enable = lib.mkEnableOption "Enable nix-centric developer environment";

    signCommits = lib.EnableOption "Sign git commits with GPG key";

    gpgKey = lib.mkOption { };

    fullname = lib.mkOption { };
    email = lib.mkOption { };
  };

  config = lib.mkIf cfg.enable (lib.mkMerge [
    {
      programs.git = {
        enable = true;
        extraConfig = {
          core = { autocrlf = "input"; };
          hub = { protocol = "https"; };
        };

        userEmail = cfg.email;
        userName = cfg.fullname;
      };
    }
    (lib.mkIf cfg.signCommits {
      programs.git = {
        signing = {
          signByDefault = true;
          # subkeys require a ! suffix
          # key = "814CF88EBD7287A1!";
          key = cfg.gpgKey;
        };
      };
    })
    { home.packages = with pkgs; [ jq ripgrep ]; }
    {
      programs.direnv = {
        enable = true;
        enableZshIntegration = true;
        nix-direnv = { enable = true; };
      };
    }
  ]);
}
