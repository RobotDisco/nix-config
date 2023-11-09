# This only impacts X and the console, as wayland compositors are supposed to configure themselves
{
  # This should work even if I don't activate X11
  console.useXkbConfig = true;

  services.xserver.xkbOptions = "ctrl:nocaps";
}
