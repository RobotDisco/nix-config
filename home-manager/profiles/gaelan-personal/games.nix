{ pkgs, ... }:

{
  home.packages = with pkgs; [
    (dwarf-fortress-packages.dwarf-fortress-full.override {
      theme = "cla";
      enableFPS = false;
    })
    nethack
    flightgear
    # Adventure-game engine
    scummvm
    # Open up GOG artifacts
    innoextract
    # Interactive Fiction engine
    frotz
  ];
}
