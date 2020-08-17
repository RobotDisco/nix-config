{ pkgs, lib, ... }:

{
  fonts = {
    enableFontDir = true;
    fonts = [] ++ lib.optionals (pkgs ? camingo-code) [
      pkgs.camingo-code
    ];
  };
}
