{ myLib, ... }:

{
  imports = [ ../../common ] ++ myLib.scanPaths ./.;
}
