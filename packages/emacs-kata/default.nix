{
  emacsWithPackagesFromUsePackage,
  emacs-pgtk,
}:

# Lean Emacs for a uConsole code-kata device. aarch64-linux only; uses
# emacs-pgtk (no darwin patches needed). See
# docs/superpowers/specs/2026-07-03-uconsole-kata-emacs-design.md
emacsWithPackagesFromUsePackage {
  package = emacs-pgtk;

  # Parse the hand-written init.el for use-package :ensure forms so the
  # build supplies exactly the Elisp packages the config loads.
  config = ./init.el;

  # Respect the :ensure keyword rather than installing every use-package.
  alwaysEnsure = false;
}
