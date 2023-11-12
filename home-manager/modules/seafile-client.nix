{ config, lib, pkgs, ... }:

let
  cfg = config.robot-disco.services.seafile-client;
in {
  options.robot-disco.services.seafile-client = {
    enable  = lib.mkEnableOption "Seafile Client";
  };

  config = lib.mkIf cfg.enable {
    systemd.user.services.seafile-client = {
      Unit = {
        Description = "Seafile Client";
        After = [ "graphical-session-pre.target" ];
        PartOf = [ "graphical-session.target" ];
      };
      Service = {
        Environment = "PATH=${config.home.profileDirectory}/bin";
        ExecStart = "${pkgs.seafile-client}/bin/seafile-applet";
      };
      Install = { WantedBy = [ "graphical-session.target" ]; };
    };
  };
}
