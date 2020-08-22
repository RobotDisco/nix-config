{ pkgs, lib, ... }:

{
  fonts = {
    enableFontDir = true;
    fonts = [
      pkgs.camingo-code
      pkgs.anonymousPro
    ];
  };
}
