# Use final for referencing dependencies
# Use prev for overriding package definitions
final: prev:

let
  # Emacs package generated from use-package s-expressions in our emacs
  # configuration file.
  runtime =
    final.emacsWithPackagesFromUsePackage {
      package = final.emacsNativeComp;

      # Parse this org file for "use-package" s-expressions to implicitly
      # import emacs-overlay nix elisp packages from.
      config = ./init.org;
      # Don't assume every "use-package" s-expression should be installed,
      # respect :ensure keyword.
      alwaysEnsure = false;

      extraEmacsPackages = (epkgs: [
        # use-package has some dependencies
        epkgs.diminish
      ]);
    };
in
{
  gaelan-emacs = runtime;
  gaelan-emacs-config = (final.emacsPackagesFor runtime.emacs).callPackage
    ./config.nix {
      packageRequires = runtime.explicitRequires;
    };
}
