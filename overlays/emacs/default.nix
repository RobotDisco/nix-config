# Use final for referencing dependencies
# Use prev for overriding package definitions
final: prev:

let
  # Emacs package generated from use-package s-expressions in our emacs
  # configuration file.
  runtime = final.emacsWithPackagesFromUsePackage {
    package = if final.stdenv.isDarwin
    then prev.emacs.overrideAttrs(old: {
      patches  =
        (old.patches or []) ++ [
          # Fix OS window role (needed for window managers like yabai)
          (final.fetchpatch {
            url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-28/fix-window-role.patch";
            sha256 = "sha256-+z/KfsBm1lvZTZNiMbxzXQGRTjkCFO4QPlEK35upjsE=";
          })
          # Don't refocus a different frame if emacs frame is closed
          (final.fetchpatch {
            url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-28/no-frame-refocus-cocoa.patch";
            sha256 = "sha256-QLGplGoRpM4qgrIAJIbVJJsa4xj34axwT3LiWt++j/c=";
          })
          # Enable rounded window with no decoration
          (final.fetchpatch {
            url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-28/no-titlebar-and-round-corners.patch";
            sha256 = "sha256-RYdjAf1c43Elh7ad4kujPnrCX8qY7ZWxufJfCc0QW00=";
          })
          # Make Emacs aware of OS-level light/dark mode
          (final.fetchpatch {
            url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-28/system-appearance.patch";
            sha256 = "sha256-oM6fXdXCWVcBnNrzXmF0ZMdp8j0pzkLE66WteeCutv8=";
          })
        ];
    })
              else final.emacs;

    # Parse this org file for "use-package" s-expressions to implicitly
    # import emacs-overlay nix elisp packages from.
    config = ./init.org;
    # Don't assume every "use-package" s-expression should be installed,
    # respect :ensure keyword.
    alwaysEnsure = false;

    extraEmacsPackages = (epkgs:
      [
        # use-package has some dependencies
        epkgs.diminish
      ]);
  };
in {
  gaelan-emacs = runtime;
  gaelan-emacs-config =
    (final.emacsPackagesFor runtime.emacs).callPackage ./config.nix {
      packageRequires = runtime.explicitRequires;
    };
}
