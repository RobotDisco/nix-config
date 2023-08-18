{
  programs.steam.enable = true;

  # A lot of games are 32-bit, which audio and video libraries don't
  # enable by default.
  services.pipewire.alsa.support32Bit = true;
}
