# home-manager module for gaelan's personal emacs
_:

{
  home.file = {
    ## Sounds for my pomodoro plugin
    emacsPomodoroStartSound = {
      source = ./audio/incoming_hail2.mp3;
      target = ".emacs.d/audio/incoming_hail2.mp3";
    };
    emacsPomodoroFinishSound = {
      source = ./audio/ds9intercom.mp3;
      target = ".emacs.d/audio/ds9intercom.mp3";
    };
    emacsPomodoroFinishLongSound = {
      source = ./audio/computerbeepsequence1.mp3;
      target = ".emacs.d/audio/computerbeepsequence1.mp3";
    };
  };
}
