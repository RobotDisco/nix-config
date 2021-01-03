{pkgs, ... }:

{
  environment.variables.EDITOR = "${pkgs.emacs}/bin/emacsclient -c";
  environment.variables.ALTERNATE_EDITOR = "${pkgs.emacs}/bin/emacs";

  environment.systemPackages = [ pkgs.emacs ];
  
  environment.shellAliases = {
    vim = "${pkgs.emacs}/bin/emacsclient -n";
    vi  = "${pkgs.emacs}/bin/emacsclient -n";
    e   = "${pkgs.emacs}/bin/emacsclient -n";
  };

  services.emacs = {
    enable = true;
    package = pkgs.emacs;
  };
}
