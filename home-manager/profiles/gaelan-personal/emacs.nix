{
  programs.emacs = {
    enable = true;
    extraPackages = epkgs: [ epkgs.use-package ];
  };

  services.emacs = {
    enable = true;
    defaultEditor = true;
  };
}
