{ config, lib, ... }:
let
  cfg = config.robot-disco.xserver;
in
{
  options.robot-disco.xserver.enable = lib.mkEnableOption "X graphical environment";

  config = lib.mkIf cfg.enable {
    services.xserver.enable = true;
  };
}
