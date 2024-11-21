{
  services.hyprpaper = {
    enable = true;
    settings = {
      ipc = "on";
      splash = false;
      splash_offset = 2.0;

      preload = [
        "${../../../../backgrounds/frieren_white.jpg}"
        "${../../../../lordran.jpg}"
        "${../../../../nier.png}"
        "${../../../../yotsugi_eyes.png}"
      ];

      wallpaper = [
        "eDP-1,${../../../../frieren_white.jpg}"
        "desc:Dell Inc. DELL U2412M HT5N364F0GSS, ${../../../../yotsugi_eyes.png}"
        "desc:Dell Inc. DELL U2412M M2GCR1CS0T1L, ${../../../../frieren_white.jpg}"

      ];
    };
  };
}
