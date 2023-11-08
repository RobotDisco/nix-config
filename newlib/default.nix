{ lib, nixpkgs }:

{
  # For all supported systems
  # given a function that takes a system string as its only input
  # return an attrset where the key is each supported system string and the
  # value is the respective result of calling our supplied function for each
  # system string.
  forAllSystems = import ./forAllSystems.nix { inherit lib nixpkgs; };
}
