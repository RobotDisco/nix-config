{ pkgs, lib, ... }:

{
  fonts = {
    enableFontDir = true;
    fonts = [
      pkgs.corefonts
      pkgs.camingo-code
      pkgs.anonymousPro
    ];
  };
}
