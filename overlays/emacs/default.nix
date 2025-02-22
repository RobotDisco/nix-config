# Use final for referencing dependencies
# Use prev for overriding package definitions
final: prev:

let
  # Emacs package generated from use-package s-expressions in our emacs
  # configuration file.
  runtime = final.emacsWithPackagesFromUsePackage {
    package = if final.stdenv.isDarwin then final.emacs-macport else final.emacs29-pgtk;

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
            inherit (prev.emacsPackages) dap-mode;
          }
        );
      };
  };
in
{
  gaelan-emacs = runtime;
  gaelan-emacs-config = (final.emacsPackagesFor runtime.emacs).callPackage ./config.nix {
    packageRequires = runtime.explicitRequires;
  };
}
