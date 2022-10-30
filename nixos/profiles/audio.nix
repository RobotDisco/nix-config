{ config, lib, ... }:

let
  cfg = config.robot-disco.hardware.audio;
in

{
  options.robot-disco.hardware.audio = {
    enable = lib.mkEnableOption "Enable hardware sound output";
  };

  config = lib.mkIf cfg.enable {
    # pipewire is the new pulseaudio
    # rtkit is optional for sound but recommended for some reason
    security.rtkit.enable = true;
    services.pipewire = {
      enable = true;
      # ALSA is the low-level audio layer of Linux.
      alsa.enable = true;
      # Enable PulseAudio compatibility
      pulse.enable = true;
      # Only needed for steam, honestly
      alsa.support32Bit = lib.mkDefault false;
    };
  };
}
