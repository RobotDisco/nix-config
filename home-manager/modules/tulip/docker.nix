{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.robot-disco.tulip;
in
lib.mkIf cfg.enable {
  home.packages = [
    # Docker VM for macs
    pkgs.docker
  ];
}
