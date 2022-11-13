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

    home.packages = with pkgs; [
      brave
      bitwarden
      seafile-client
      slack

      # Book reading
      calibre
      unzip
    ];
  };
}
