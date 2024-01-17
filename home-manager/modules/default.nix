{ lib, pkgs, ... }:

{
  imports = [
    ./development-environment.nix
    ./emacs.nix
    ./gnupg.nix
    ./seafile-client.nix
    ./sway.nix
    ./tulip
  ];
}
