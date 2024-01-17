{ pkgs, ... }:

{
  programs.emacs = {
    enable = true;
    package = pkgs.emacs29;
    extraPackages = epkgs: [ epkgs.use-package ];
  };

  xdg.configFile."emacs/init.el".source = ./init.el;
}
