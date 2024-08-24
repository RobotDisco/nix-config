# Remapping my kensington expert mouse to have a button layout I prefer
{
  # Remap buttons for my kensington trackball
  # NOTE For xserver config limitation reasons I can see this moving into a
  # dedicated module, it doesn't seem composable.
  services.xserver.config = ''
    Section "InputClass"
      Identifier "Kensington Expert Mouse"
      MatchUSBID "047d:1020"
      Driver "libinput"
      Option "ButtonMapping" "1 2 8 4 5 6 7 3 9"
    EndSection
  '';
}
