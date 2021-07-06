{ pkgs, ... }:

{
  fonts = {
    # fontDir.enable = true;
    enableFontDir = true;
    fonts = [
      pkgs.corefonts
      pkgs.camingo-code
      pkgs.anonymousPro
      pkgs.lato
    ];
  } // (if pkgs.stdenv.isLinux
        then { fontDir.enable = true; }
        else { enableFontDir = true; }); 
}
