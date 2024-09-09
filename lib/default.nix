{ lib, ... }:

{
  scanPaths = import ./scanPaths.nix { inherit lib; };
}
