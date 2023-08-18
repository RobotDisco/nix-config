# home-manager module for gaelan's personal emacs
{ lib, pkgs, ... }:

{
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
}
