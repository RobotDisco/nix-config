{
  config,
  lib,
  pkgs,
  ...
}:
let
  emacsPkg = config.programs.emacs.finalPackage;
in
lib.mkMerge [
  {
    programs.emacs = {
      enable = true;
      package = if pkgs.stdenv.isDarwin then pkgs.emacs29-macport else pkgs.emacs29-pgtk;
      extraPackages = epkgs: [
        epkgs.diminish
        epkgs.use-package
        epkgs.vterm
      ];
    };

    xdg.configFile."emacs/init.el".source = ./init.el;

    home.packages = [
      emacsPkg
      # Dictionary support
      pkgs.aspell
      pkgs.aspellDicts.en
      pkgs.aspellDicts.en-science
      pkgs.aspellDicts.en-computers
      # org-roam graph support
      pkgs.graphviz
      # Graphics support
      pkgs.imagemagick
      # LaTeX support
      pkgs.texlive.combined.scheme-full
      # ePub support
      pkgs.unzip
    ];
  }
  (lib.mkIf pkgs.stdenv.isDarwin {
    programs.zsh.shellAliases.emacs = "${emacsPkg}/Applications/Emacs.app/Contents/MacOS/Emacs";
  })
]
