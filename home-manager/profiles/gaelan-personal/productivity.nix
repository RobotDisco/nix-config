{ pkgs, ... }:

{
  home.packages = with pkgs; [
    calibre
    # Sunsama is currently a package installed via personal overlay
    sunsama
    zotero
  ];
}
