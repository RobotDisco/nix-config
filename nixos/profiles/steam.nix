{ config, lib, ... }:

let
  cfg = config.robot-disco.software.steam;
in
{
  options.robot-disco.software.steam = {
    enable = lib.mkEnableOption "Install the Steam videogame store.";
  };

  config = lib.mkIf cfg.enable {
    programs.steam = {
      enable = true;
    };

    # A lot of games are 32-bit, which audio and video libraries don't
    # enable by default.
    services.pipewire = {
      alsa.support32Bit = lib.mkForce true;
    };
  };
}
