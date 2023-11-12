{ lib, pkgs, ... }:

{
  imports = [
    ./development-environment.nix
    ./emacs.nix
    ./gnupg.nix
    ./seafile-client.nix
    ./tulip
  ];
}
