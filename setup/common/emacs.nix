{pkgs, ... }:

{
  environment.variables.EDITOR = "${pkgs.emacs}/bin/emacsclient -tc";
  environment.variables.ALTERNATE_EDITOR = "${pkgs.emacs}/bin/emacs";

  environment.systemPackages = [ pkgs.emacs ];
  
  environment.shellAliases = {
    vim = "${pkgs.emacs}/bin/emacsclient -nw";
    vi  = "${pkgs.emacs}/bin/emacsclient -nw";
    e   = "${pkgs.emacs}/bin/emacsclient -nw";
  };
}
