# Gaelan's user configuration
{ config, pkgs, lib, ... }:
let cfg = config.robot-disco.user.gaelan;
in {
  options.robot-disco.user.gaelan = {
    enable = lib.mkEnableOption "Enable gaelan user config";
  };

  config = lib.mkIf cfg.enable {
    xdg.enable = true;

    robot-disco.emacs = {
      enable = true;

      enableServer = true;
      enableExwm = true;
    };

    programs.home-manager.enable = true;

    programs.ssh = {
      enable = true;
      compression = true;
      # Don't forward by default, it is insecure
      # Prefer proxyjumping if you can
      forwardAgent = false;
    };

    programs.zsh = {
      # Since I use zsh, make sure home-manager sets it up.
      enable = true;
    };

    # install and configure git
    programs.git = {
      enable = true;
      extraConfig = {
        core = { autocrlf = "input"; };
        hub = { protocol = "https"; };
      };

      signing = {
        signByDefault = true;
        # subkeys require a ! suffix
        key = "814CF88EBD7287A1!";
      };

      # TODO How do I get work laptop pointing to work email?
      userEmail = "gdcosta@gmail.com";
      userName = "Gaelan D'costa";
    };

    # Temporarily turn off but should only be on for laptop anyway 
    services.screen-locker = {
      enable = true;
      lockCmd = "${pkgs.i3lock}/bin/i3lock -c 746542";
    };

    home.keyboard = {
      layout = "us";
      options = [ "ctrl:nocaps" ];
    };

    home.packages = with pkgs; [
      brave
      bitwarden
      networkmanagerapplet
      seafile-client
      slack

      # These are part of my developer portfolio
      jq
      ripgrep

      # Book reading
      calibre
      unzip
    ];

    programs.direnv = {
      enable = true;
      enableZshIntegration = true;
      nix-direnv = {
        enable = true;
      };
    };
  };
}
