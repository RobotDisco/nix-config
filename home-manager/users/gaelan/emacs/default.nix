# home-manager module for gaelan's personal emacs
{ pkgs, ... }:

let
  emacsEnv = pkgs.emacsWithPackagesFromUsePackage {
    config = ./init.org;
  };
  # init-el = pkgs.emacs.trivialBuild {
  #   pname = "init-el";
  #   src = lib.sourceByRegex ./emacs [ "*.org" ];

  #   preBuild = ''
  #     # Tangle org files
  #     emacs --batch -Q \
  #       -l org \
  #       *.org \
  #       -f org-babel-tangle

  #     # Fake config directory in order to have files on load-path
  #     mkdir -p .xdg-config
  #     ln -s $PWD .xdg-config/emacs
  #     export XDG_CONFIG_HOME="$PWD/.xdg-config"

  #     emacs --batch -Q \
  #       -l package \
  #       -eval '(setq package-quickstart t)' \
  #       -f package-quickstart-refresh
  #   '';
  # };
in

{
  # Install additional packages like system tray + status bar
  home.packages = with pkgs; [
    stalonetray
    xmobar
  ];
  
  ## Install our emacs
  programs.emacs = {
    enable = true;
    package = emacsEnv;
  };
  
  ## Emacs config needs copying to my homedir
  home.file.emacsConfig = {
    #sourceFile = "${init-el}/.xdg-config/emacs/init.el";
    source = ./init.el;
    target = ".emacs.d/init.el";
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
  xsession.enable = true;
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
