{
  programs.emacs = {
    enable = true;
  };

  xdg.configFile."emacs/init.el".source = ./init.el;
}
