{ myLib, ... }:
# Support for various devices found around the house
{
  imports = myLib.scanPaths ./.;
}
