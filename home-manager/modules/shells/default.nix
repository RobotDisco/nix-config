{ config, ... }:

{
  programs = {
    dircolors.enable = true;

    zsh = {
      enableCompletion = true;
      enableVteIntegration = true;

      dotDir = "${config.xdg.configHome}/zsh";

      syntaxHighlighting = {
        enable = true;
        highlighters = [
          # Default, loading explicitly.
          "main"
          # Highlight parens
          "brackets"
          # Highlight user-defined glob patterns
          #"pattern"
          # Highlight used-defined regular expressions
          #"regexp"
          # Highlight the cursor
          # "cursor"
          # Highlight the entire line when you are root
          "root"
          # Highlight the whole line?
          # "line"
        ];
        styles = {
          root = "fg=red,bold";
        };
      };
      history = {
        # Allow multiple concurrent zsh sessions to append to the history
        # rather than the latest one overwriting the file.
        append = true;
        # Expire duplicate commands first rather than straighforwardly
        # expiring the earliest entries.
        expireDuplicatesFirst = true;
        # Save timestamps in the history file (unsure if this is a good idea)
        extended = true;
        # Don't show duplicates of a command when searching the history.
        # findNoDups = true; (enable in 25.05)
        # Ignore consequtively run duplicate commands from the in-memory
        # history. This will still keep them if you run a different command
        # in-between.
        ignoreDups = true;
        # Don't save duplicate runs into the history file.
        # saveNoDups = true; (enable in 25.05)
      };

      # All zsh logic lives in $ZDOTDIR/rc.zsh (options, zstyles,
      # navigation, prompt) and $ZDOTDIR/functions/ (autoloaded
      # functions, one per file). initContent is just the wiring:
      # register the functions directory on fpath, autoload each
      # file in it, then source rc.zsh.
      #
      # ZDOTDIR is exported by home-manager because dotDir is set.
      initContent = ''
        fpath=("$ZDOTDIR/functions" $fpath)
        for fn in "$ZDOTDIR/functions"/*(N:t); do
          autoload -Uz -- "$fn"
        done
        [[ -r "$ZDOTDIR/rc.zsh" ]] && source "$ZDOTDIR/rc.zsh"
      '';
    };
  };

  xdg.configFile = {
    "zsh/rc.zsh".source = ./files/rc.zsh;
    "zsh/functions".source = ./files/functions;
  };
}
