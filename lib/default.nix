{ lib, nixpkgs, ... }:

{
    host = import ./host.nix { inherit lib; inherit nixpkgs; };
}