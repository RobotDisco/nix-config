{ myLib, ... }:

{
  imports = [
    ../common

    ../hardware/ssd.nix
  ]
  ++ myLib.scanPaths ./.;
}
