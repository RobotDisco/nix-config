{ config, lib, pkgs, ... }:

let
  cfg = config.services.seafile-client;
in {
  options = {
    services.seafile-client = {
      enable  = lib.mkEnableOption "Seafile Client";

      package = lib.mkOption {
        type = lib.types.package;
        default = pkgs.seafile-client;
        defaultText = lib.literalExpression "pkgs.seafile-client";
        description = "The package to use for the seafile client binary.";
      };
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      (lib.hm.assertions.assertPlatform "services.seafile-client" pkgs lib.platforms.linux)
    ];

    systemd.user.services.seafile-client = {
      Unit = {
        Description = "Seafile Client";
        After = [ "graphical-session-pre.target" ];
        PartOf = [ "graphical-session.target" ];
      };
      Service = {
        Environment = "PATH=${config.home.profileDirectory}/bin";
        ExecStart = "${cfg.package}/bin/seafile-applet";
      };
      Install = { WantedBy = [ "graphical-session.target" ]; };
    };
  };
}
