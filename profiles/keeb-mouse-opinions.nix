{
  # Honour the same settings in the linux console as in X11
  console.useXkbConfig = true;

  # Caps Lock must die; replace with Ctrl
  services.xserver.xkbOptions = "ctrl:nocaps";

  # Enable touchpad support.
  services.xserver.libinput.enable = true;
  # Turn off touchpad tapping, it is very annoying and disruptive
  services.xserver.libinput.touchpad.tapping = false;

  # Enable trackpoint support and enable scrolling.
  hardware.trackpoint.enable = true;
  hardware.trackpoint.emulateWheel = true;

  # Remap buttons for my kensington trackball
  services.xserver.config = ''
Section "InputClass"
        Identifier "Kensington Expert Mouse"
        MatchUSBID "047d:1020"
        Driver "libinput"
        Option "ButtonMapping" "1 2 8 4 5 6 7 3 9"
EndSection
  '';
}
