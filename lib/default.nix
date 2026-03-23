# This function receives inputs from flake.nix and re-exports our library functions.
# We use a common Nix pattern here: accepting more arguments than we need and
# passing the full argument set (@args) to other functions.
{
  lib,
  # The "..." means "accept any otheremacs arguments and ignore them"
  # This is safe and allows us to pass extra arguments without errors
  ...
}@args: # @args captures the ENTIRE input argument set for forwarding

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

  /*
    Build NixOS system configuration with sensible defaults.
    See lib/nixosSystem.nix for full documentation.

    Note: We pass the full 'args' set here, even though nixosSystem.nix only
    needs some of these arguments. This is a safe and common Nix pattern:
    - Functions extract only the parameters they declare
    - Extra arguments are ignored due to the "..." in parameter sets
    - This allows consistent argument passing without manual filtering
  */
  nixosSystem = import ./nixosSystem.nix args;

  /*
    Build macOS system configuration with sensible defaults.
    See lib/darwinSystem.nix for full documentation.

    Same pattern as nixosSystem above - we pass all arguments and let
    darwinSystem.nix extract what it needs.
  */
  darwinSystem = import ./darwinSystem.nix args;
}
