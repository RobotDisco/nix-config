{ config, pkgs, lib, ... }:

let

  emacs = pkgs.emacsWithPackagesFromUsePackage {
    config = ./emacs/init.org;
  };

in

{
    users.users.gaelan = {
      isNormalUser = true;
      home = "/home/gaelan";
      description = "Gaelan D'costa";
      # TODO if I have role profiles elsewhere, these should be confingured there
      extraGroups = [ "wheel" "networkmanager" "docker" "video" ];
    };

    environment.variables.EDITOR = "${emacs}/bin/emacsclient -c";
    environment.variables.ALTERNATE_EDITOR = "${emacs}/bin/emacs";
    environment.systemPackages = [ emacs ];

    environment.shellAliases = {
      vim = "${emacs}/bin/emacsclient -n";
      vi  = "${emacs}/bin/emacsclient -n";
      e   = "${emacs}/bin/emacsclient -n";
    };
}
