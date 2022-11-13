# Gaelan's user configuration
{ config, pkgs, lib, ... }:
let cfg = config.robot-disco.user.gaelan;
in {
  options.robot-disco.user.gaelan = {
    enable = lib.mkEnableOption "Enable gaelan user config";
  };

  config = lib.mkIf cfg.enable {
    robot-disco.emacs = {
      enable = true;

      enableServer = true;
      enableExwm = true;
    };

    # Temporarily turn off but should only be on for laptop anyway 
    services.screen-locker = {
      enable = true;
      lockCmd = "${pkgs.i3lock}/bin/i3lock -c 746542";
    };

    home.packages = with pkgs; [
      brave
      bitwarden
      networkmanagerapplet
      seafile-client
      slack

      # Book reading
      calibre
      unzip
    ];
  };
}
