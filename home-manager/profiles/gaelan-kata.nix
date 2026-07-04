{ pkgs, ... }:

# Barebones home-manager profile for a uConsole code-kata device.
# Intentionally NOT a module: this is a single-machine composition with no
# behaviour that varies across profiles, so it has no `enable` option.
# The kata Emacs is launched interactively (no services.emacs daemon), so
# $PATH carries the toolchains below and the config needs no store paths.
let
  emacsKata = pkgs.callPackage ../../packages/emacs-kata { };
in
{
  home.packages = with pkgs; [
    emacsKata
    # Lisp
    guile
    clojure
    clj-kondo
    zprint
    # Elm
    elmPackages.elm
    elmPackages.elm-format
    elmPackages.elm-language-server
    # zsh linting (flycheck)
    shellcheck
    # editor-invoked tools
    ripgrep
    git
    # typeface
    nerd-fonts.iosevka
  ];

  # Deploy the hand-written config verbatim — no tangle, no substitution.
  xdg = {
    enable = true;
    configFile."emacs/init.el".source = ../../packages/emacs-kata/init.el;
    configFile."emacs/early-init.el".source = ../../packages/emacs-kata/early-init.el;
  };
}
