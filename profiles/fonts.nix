{ pkgs, ... }:

{
  fonts = {
    fonts = [
      # A nice default spread of fonts, albeit stolen from Microsoft
      pkgs.corefonts
      # My current fav programming typeface
      pkgs.camingo-code
      # My old fav programming typeface, still used in places
      pkgs.anonymousPro
      # I must have needed this font for some reason.
      pkgs.lato
    ];
  } // (if pkgs.stdenv.isLinux
        then { fontDir.enable = true; }
        else { enableFontDir = true; });
}
