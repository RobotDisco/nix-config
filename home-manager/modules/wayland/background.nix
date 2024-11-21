let
  landscape-background = toString ../../../backgrounds/frieren_white.jpg;
  portrait-background = toString ../../../backgrounds/lordran.jpg;
in
{
  services.hyprpaper = {
    enable = true;
    settings = {
      ipc = "on";
      splash = true;
      splash_offset = 2.0;

      preload = [
        landscape-background
        portrait-background
      ];

      wallpaper = [
        "eDP-1,${landscape-background}"
        "desc:Dell Inc. DELL U2412M M2GCR1CS0T1L, ${landscape-background}"
        "desc:Dell Inc. DELL U2412M HT5N364F0GSS, ${portrait-background}"
      ];
    };
  };
}
