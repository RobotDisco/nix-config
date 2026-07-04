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
  nixpkgs.overlays = [ (import <emacs-overlay>) ];

  # Home Manager needs a bit of information about you and the paths it should
  # manage.
  home.username = "gaelan";
  home.homeDirectory = "/home/gaelan";

  # This value determines the Home Manager release that your configuration is
  # compatible with. This helps avoid breakage when a new Home Manager release
  # introduces backwards incompatible changes.
  #
  # You should not change this value, even if you update Home Manager. If you do
  # want to update the value, then make sure to first check the Home Manager
  # release notes.
  home.stateVersion = "26.05"; # Please read the comment before changing.

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

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
