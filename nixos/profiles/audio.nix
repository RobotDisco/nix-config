# Enable audio support via the venerable pulseaudio
{ pkgs, ... }:

{
  # Apparently pulseaudio doesn't like this
  # sound.enable = true;

  # Use pulseaudio for unified sound control
  # hardware.pulseaudio = {
  #   enable = true;
  # };

  environment.systemPackages = with pkgs; [
    alsaUtils
  ];

  # Use pipewire for audio/video
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };
}
