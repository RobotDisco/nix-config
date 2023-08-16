{ config, lib, ... }:

let cfg = config.robot-disco.hidpi;
in {
  options.robot-disco.hidpi.enable = lib.mkEnableOption {
    description = "Enable HiDPI support.";
  };

  config = lib.mkIf cfg.enable {
    services.xserver.dpi = 200;
    services.xserver.upscaleDefaultCursor = true;
  };
}
