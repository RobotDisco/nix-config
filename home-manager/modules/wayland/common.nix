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

    services = {
      # Persist clipboard contents after source window loses focus
      wl-clip-persist.enable = true;
      # Notification popup messages
      mako.enable = true;
    };

    home.packages = with pkgs; [
      # Adjust screen brightness (used in lock-screen idle timeouts)
      brightnessctl
      # Wayland screenshot capture
      grim
      # Interactive screen region selector for grim
      slurp
      # Provides loginctl, used to trigger lock-session
      util-linux
      # wl-copy/wl-paste: Wayland clipboard tools used by Emacs
      wl-clipboard
      # wpctl for volume control keybindings
      wireplumber
    ];
  };
}
