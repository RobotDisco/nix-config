{
  emacsWithPackagesFromUsePackage,
  baseEmacs,
  emacsPackages,
}:

emacsWithPackagesFromUsePackage {
  package = baseEmacs;

  # Parse this org file for "use-package" s-expressions to implicitly
  # import emacs-overlay nix elisp packages from.
  config = ./init.org;
  # Don't assume every "use-package" s-expression should be installed,
  # respect :ensure keyword.
  alwaysEnsure = false;

  extraEmacsPackages = epkgs: [
    epkgs.use-package
    # use-package has some dependencies
    epkgs.diminish
    # Include tree-sitter grammars
    epkgs.treesit-grammars.with-all-grammars
  ];

  override =
    epkgs:
    epkgs
    // {
      lsp-java = epkgs.lsp-java.override (
        epkgs:
        epkgs
        // {
          inherit (emacsPackages) dap-mode;
        }
      );
    };
}
