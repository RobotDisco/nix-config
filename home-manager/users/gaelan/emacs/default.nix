# home-manager module for gaelan's personal emacs
{ lib, pkgs, ... }:

let
  emacsP = pkgs.emacsWithPackagesFromUsePackage {
    config = ./init.org;
  };
  emacsConfig = pkgs.stdenv.mkDerivation {
    ## I am indebted to
    ## https://github.com/terlar/emacs-config/blob/main/default.nix
    ## for insight into how to autogenerate a tangled org
    ## as part of a nix derivation.
    name = "gaelan-emacs-config";

    src = lib.sourceByRegex ./. ["init.org"];

    buildInputs = [ emacsP ];
    buildPhase = ''
      # Generate elisp from org file
      # We need to load our org file in the load path
      emacs --batch -Q \
        -l org \
        init.org \
        -f org-babel-tangle

      # Fake config directory in order to have file in load-path
      mkdir -p .xdg-config
      ln -s $PWD .xdg-config/emacs
      export XDG_CONFIG_HOME="$PWD/.xdg-config"

      # Refresh emacs-quickstart package deferral
      emacs --batch -Q \
        -l package \
        -eval '(setq package-quickstart t)' \
        -f package-quickstart-refresh
    '';
    installPhase = ''
      # Export generated elisp files for home-manager to install
      install -D -t $out $PWD/*.el
    '';
  };

in

{
  # Install additional packages like system tray + status bar
  home.packages =
    if pkgs.stdenv.isDarwin
    then []
    else with pkgs; [
      stalonetray
      xmobar
    ];
  
  ## Install our emacs
  programs.emacs = {
    enable = true;
    package = emacsP;
  };
  
  ## Emacs config needs copying to my homedir
  home.file.emacsConfig = {
    source = "${emacsConfig}/init.el";
    # source = ./init.el;
    target = ".emacs.d/init.el";
  };
  home.file.packageQuickstartElisp = {
    source = "${emacsConfig}/package-quickstart.el";
    target = ".emacs.d/package-quickstart.el";
  };
  
  # TODO this is insecure because /nix/store stores this.
  # Use SOP to fix
  home.file.emacsSecrets = {
    source = ./secrets.el;
    target = ".emacs.d/secrets.el";
  };

  ## Sounds for my pomodoro plugin
  home.file.emacsPomodoroStartSound = {
    source = ./audio/incoming_hail2.mp3;
    target = ".emacs.d/audio/incoming_hail2.mp3";
  };
  home.file.emacsPomodoroFinishSound = {
    source = ./audio/ds9intercom.mp3;
    target = ".emacs.d/audio/ds9intercom.mp3";
  };
  home.file.emacsPomodoroFinishLongSound = {
    source = ./audio/computerbeepsequence1.mp3;
    target = ".emacs.d/audio/computerbeepsequence1.mp3";
  };

  
  # I use emacs as my window manager, which makes things funky :D
  # TODO this possibly should be a custom package
  # We're leveraging .xsession support to load our window manager
  xsession.enable = if pkgs.stdenv.isLinux then true else false;
  xsession.windowManager.command = "emacs";
    
  ## Put down configuration for my system tray + systembar
  home.file.stalonetrayConfig = {
    source = ./stalonetrayrc;
    target = ".stalonetrayrc";
  };
  home.file.xmobarConfig = {
    source = ./xmobarrc;
    target = ".xmobarrc";
  };
}
