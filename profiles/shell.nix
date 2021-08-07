# My current shell of choice (outside Emacs, at least) is zsh
# This should be home-manager stuff honestly

{ pkgs, ... }:

{  
  programs.zsh = {
    enable = true;
    # Bash compatibility
    # enableBashCompletion = true;
    enableCompletion = true;
    # enableFzfGit = true;
    # enableFzfHistory = true;
  } // (if pkgs.stdenv.isLinux
        then { syntaxHighlighting.enable = true; }
        else { enableSyntaxHighlighting = true; });

  environment.shellAliases = {
    ls = "ls -FGh";
    pgrep="pgrep -a";
    grep = "grep --colour=auto";
    nix-install = "nix-env -f '<nixpkgs>' -iA";
  };
}
