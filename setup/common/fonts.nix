{ pkgs, lib, ... }:

{
  fonts = {
    fontDir.enable = true;
    fonts = [
      pkgs.corefonts
      pkgs.camingo-code
      pkgs.anonymousPro
      pkgs.lato
    ];
  };
}
