{ config, lib, ... }:

let
  cfg = config.robot-disco.window-manager;
in
{
  options.robot-disco.window-manager = {
    enable = lib.mkEnableOption "Enable Graphical Environment";
  };

  config = lib.mkIf cfg.enable {
    services.xserver = {
      enable = true;
    };
  };
}
