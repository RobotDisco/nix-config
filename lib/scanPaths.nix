{ lib, ... }:

path:

builtins.map
  # Higher order function to produce a relative path
  # from a relative filename, assuming a pre-supplied current path
  (f: (path + "/${f}"))
  # Get the list of names of an attrset.
  (
    builtins.attrNames
      # Include directories (for recursive importing)
      # and nix files not named "default.nix" (which is covered in the above case)
      (
        lib.attrsets.filterAttrs
          (
            path: type:
            (type == "directory") # include directories
            || (
              (path != "default.nix") # ignore default.nix
              && (lib.strings.hasSuffix ".nix" path)
            ) # include .nix files
          )
          # Return attrset containing an attr for each entry in the given
          # directory, the name as key, the value as a string indicating the
          # type.
          (builtins.readDir path)
      )
  )
