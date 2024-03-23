{ config, lib, pkgs, ... }:
let emacsPkg = config.programs.emacs.finalPackage;
in lib.mkMerge [
  {
    programs.emacs = {
      enable = true;
      package =
        if pkgs.stdenv.isDarwin then pkgs.emacs-macport else pkgs.emacs-pgtk;
      extraPackages = epkgs: [ epkgs.diminish epkgs.use-package ];
    };

    xdg.configFile."emacs/init.el".source = ./init.el;

    home.packages = [
      emacsPkg
      pkgs.aspell
      pkgs.aspellDicts.en
      pkgs.aspellDicts.en-science
      pkgs.aspellDicts.en-computers
    ];
  }
  (lib.mkIf pkgs.stdenv.isDarwin {
    programs.zsh.shellAliases.emacs =
      "${emacsPkg}/Applications/Emacs.app/Contents/MacOS/Emacs";
  })
]
