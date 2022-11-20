{ pkgs }:

# Emacs package generated from use-package s-expressions in our emacs
# configuration file.
pkgs.emacsWithPackagesFromUsePackage {
  package = pkgs.emacsNativeComp;

  # Parse this org file for "use-package" s-expressions to implicitly import
  # emacs-overlay nix elisp packages from.
  config = ./init.org;
  # Don't assume every "use-package" s-expression should be installed,
  # respect :ensure keyword.
  alwaysEnsure = false;

  extraEmacsPackages = (epkgs: [
    # use-package has some dependencies
    epkgs.diminish
  ]);
}
