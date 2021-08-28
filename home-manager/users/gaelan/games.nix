# Games worth playing
{ pkgs, ... }:

{
  home.packages = with pkgs; [
    (dwarf-fortress-packages.dwarf-fortress-full.override {
      theme = "cla";
      enableFPS = false;
    })
    nethack
  ];
}
