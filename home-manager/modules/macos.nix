{ lib, pkgs, ... }:

lib.mkIf pkgs.stdenv.isDarwin {
  programs.aerospace = {
    enable = false;
  };

  home.shellAliases = {
    rm = "trash";
  };
}
