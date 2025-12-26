{
  emacsWithPackagesFromUsePackage
, emacs-macport
, emacs-pgtk
, fetchpatch
, stdenv
}:

let
  macEmacs = emacs-macport;
in
emacsWithPackagesFromUsePackage {
  package = if stdenv.isLinux then emacs-pgtk else macEmacs;

  # Parse this org file for "use-package" s-expressions to implicitly
  # import emacs-overlay nix elisp packages from.
  config = ./init.org;
  # Don't assume every "use-package" s-expression should be installed,
  # respect :ensure keyword.
  alwaysEnsure = false;

  # Use this to install packages that also include non-elisp components
  # and thus cannot be installed just via emacs config parsing.
  extraEmacsPackages = epkgs: [
    # tree-sitter grammers Used by LSP mode
    epkgs.treesit-grammars.with-all-grammars
  ];
}
