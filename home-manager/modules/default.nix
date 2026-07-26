{ myLib, pkgs, ... }:

{
  imports = myLib.scanPaths ./.;

  config.home.packages = [
    pkgs.orgnote-cli
  ];
}
