{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) types;
  cfg = config.robot-disco.tulip;

in
{
  options.robot-disco.tulip.docker = {
    enable = lib.mkOption {
      default = cfg.enable;
      description = "Whether to enable Tulip docker work environment.";
      type = types.bool;
    };
  };

  config = lib.mkIf (cfg.enable && cfg.docker.enable) {
    home.packages = [
      # Docker VM for macs
      pkgs.docker
    ];
  };
}
