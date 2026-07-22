{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.robot-disco.services.uhk-agent;
in
{
  options.robot-disco.services.uhk-agent = {
    enable = lib.mkEnableOption "UHK Agent";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.uhk-agent ];

    systemd.user.services.uhk-agent = {
      Unit = {
        Description = "UHK Agent";
        After = [ "graphical-session-pre.target" ];
        PartOf = [ "graphical-session.target" ];
      };
      Service = {
        Environment = "PATH=${config.home.profileDirectory}/bin";
        ExecStart = "${pkgs.uhk-agent}/bin/uhk-agent";
      };
      Install = {
        WantedBy = [ "graphical-session.target" ];
      };
    };
  };
}
