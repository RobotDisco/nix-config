{ pkgs ? import <nixpkgs> { overlays = [(import (builtins.fetchTarball {
  url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
}))];} }:

let

  myEmacsConfig = ./config.el;

in

pkgs.emacsWithPackagesFromUsePackage {
  config = builtins.readFile myEmacsConfig;
  alwaysEnsure = true;
}
