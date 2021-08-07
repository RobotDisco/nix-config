# Games worth playing
{ pkgs }:

{
  home.packages = with pkgs; [
    dwarf-fortress-packages.dwarf-fortress-full
    nethack
  ];
}
