# Configure emacs, my editor of choice
{pkgs, emacs-overlay, ... }:

let
  emacs = pkgs.emacs;
in
    
{
  environment.variables.EDITOR = "${emacs}/bin/emacsclient -c";
  environment.variables.ALTERNATE_EDITOR = "${emacs}/bin/emacs";

  environment.systemPackages = [ emacs ];
  
  environment.shellAliases = {
    vim = "${emacs}/bin/emacsclient -n";
    vi  = "${emacs}/bin/emacsclient -n";
    e   = "${emacs}/bin/emacsclient -n";
  };
}
  
