{ config, lib, ... }:

let
  cfg = config.robot-disco.software.window-manager;
in
{
  options.robot-disco.software.window-manager = {
    enable = lib.mkEnableOption "Enable Graphical Environment";
  };

  config = lib.mkIf cfg.enable {
    services.xserver = {
      enable = true;
    };
  };
}
