{ lib, ... }:

{
  /*
    Auto-discover .nix files and directories for module importing.

    Type: scanPaths :: Path -> [Path]

    Scans a directory and returns a list of paths suitable for importing.
    Includes .nix files (except default.nix) and subdirectories.

    Example:
      imports = myLib.scanPaths ./.;
  */
  scanPaths = import ./scanPaths.nix { inherit lib; };
}
