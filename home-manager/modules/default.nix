{ myLib, ... }:

{
  imports = [
    ../../secrets/agenix-rekey.nix
  ]
  ++ myLib.scanPaths ./.;
}
