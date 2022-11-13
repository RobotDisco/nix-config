{ config, lib, ... }:

let cfg = config.robot-disco.audio;

in {
  options.robot-disco.audio = {
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
    };
  };
}
