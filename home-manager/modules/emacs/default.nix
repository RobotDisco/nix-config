{ pkgs, ... }:

{
  programs.emacs = {
    enable = true;
    package = if pkgs.stdenv.isDarwin
              then pkgs.emacs-macport
              else pkgs.emacs-pgtk;
    extraPackages = epkgs: [ epkgs.use-package ];
  };

  xdg.configFile."emacs/init.el".source = ./init.el;
}
