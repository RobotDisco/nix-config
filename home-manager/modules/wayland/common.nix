{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.robot-disco.wayland;
in
{
  options.robot-disco.wayland.enable = lib.mkEnableOption "Common wayland desktop tools shared across compositors.";

  config = lib.mkIf cfg.enable {
    robot-disco.wayland.wallpaper.enable = true;

    programs.rofi.enable = true;

    services = {
      # Clipboard history
      cliphist.enable = true;
      # Notification popup messages
      mako.enable = true;
    };

    home.packages = with pkgs; [
      brightnessctl
      util-linux
      wireplumber
    ];
  };
}
