{ lib, ... }:

{
  programs.steam = {
    enable = true;
  };

  # A lot of games are 32-bit, so make sure to enable support
  # in audio and video
  services.pipewire = {
    alsa.support32Bit = lib.mkForce true;
  };
}