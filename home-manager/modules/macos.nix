{ lib, pkgs, ... }:

lib.mkIf pkgs.stdenv.isDarwin {
  home.shellAliases = {
    rm = "trash";
  };
}
