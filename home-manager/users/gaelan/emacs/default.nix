# home-manager module for gaelan's personal emacs
{ lib, pkgs, ... }:

let
  emacs = pkgs.emacsEnv;
  emacs-config = pkgs.emacsConfig;
in

{
  # Install additional packages like system tray + status bar
  home.packages =
    if pkgs.stdenv.isDarwin
    then with pkgs; [
      imagemagick
    ]
    else with pkgs; [
      imagemagick
      stalonetray
      xmobar
    ];
  
  ## Install our emacs
  programs.emacs = {
    enable = true;
    package = emacs;
  };
  
  ## Emacs config needs copying to my homedir
  home.file.emacsConfig = {
    source = "${emacs-config}/init.el";
    # source = ./init.el;
    target = ".emacs.d/init.el";
  };
  home.file.packageQuickstartElisp = {
    source = "${emacs-config}/package-quickstart.el";
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
  xsession.windowManager.command = "emacs -f exwm-enable";
    
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
