{ lib, ... }:

{
  /*
    Apply a function across multiple system architectures.

    Type: forEachSystem :: [String] -> (String -> a) -> AttrSet String a

    Takes a list of system strings and a function, then produces an attribute
    set where keys are system strings and values are the result of applying
    the function to each system string.

    This is a wrapper around lib.genAttrs for better semantic clarity when
    working with Nix system architectures.

    Example:
      forEachSystem ["x86_64-linux" "aarch64-darwin"] (system: "Hello ${system}")
      => {
        x86_64-linux = "Hello x86_64-linux";
        aarch64-darwin = "Hello aarch64-darwin";
      }
  */
  forEachSystem = systems: func: lib.genAttrs systems func;

  /*
    Apply a function across all supported system architectures.

    Type: forAllSystems :: (String -> a) -> AttrSet String a

    Convenience function that applies forEachSystem with a predefined list
    of supported systems. This is a curried function with the system list
    partially applied.

    The supported systems are: ["x86_64-linux" "aarch64-darwin"]

    Example:
      forAllSystems (system: nixpkgs.legacyPackages.${system}.hello)
      => {
        x86_64-linux = <hello derivation for linux>;
        aarch64-darwin = <hello derivation for darwin>;
      }
  */
  forAllSystems =
    let
      supportedSystems = [
        "x86_64-linux"
        "aarch64-darwin"
      ];
    in
    func: lib.genAttrs supportedSystems func;
}
