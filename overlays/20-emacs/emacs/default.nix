{ pkgs ? import <nixpkgs> { overlays = [(import (builtins.fetchTarball {
  url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
}))];} }:

pkgs.emacsWithPackagesFromUsePackage {
  config = ./config.org;
  alwaysEnsure = true;
}
