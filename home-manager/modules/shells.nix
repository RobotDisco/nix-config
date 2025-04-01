{
  programs = {
    dircolors.enable = true;

    zsh = {
      enableCompletion = true;
      enableVteIntegration = true;

      dotDir = ".config/zsh";

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
        # Allow multiple concurrent zsh sessions to append to the history rather
        # than the latest one overwriting the file.
        append = true;
        # Expire duplicate commands first rather than straighforwardly expiring
        # the earliest entries.
        expireDuplicatesFirst = true;
        # Save timestamps in the history file (unsure if this is a good idea)
        extended = true;
        # Don't show duplicates of a command when searching the history.
        # findNoDups = true; (enable in 25.05)
        # Ignore consequtively run duplicate commands from the in-memory history
        # This will still keep them if you run a different command in-between
        ignoreDups = true;
        # Don't save duplicate runs into the history file.
        # saveNoDups = true; (enable in 25.05)
      };

      # %h - number to reference this command in history
      # %~2 - working directory, collapsed if tilde, max two trailing components
      initExtra = ''
        export PS1="!%h %2~ $ "
      '';
    };
  };
}
