{
  emacsWithPackagesFromUsePackage,
  emacs-pgtk,
  fetchpatch,
  stdenv,
}:

let
  macEmacs = emacs-pgtk.overrideAttrs (old: {
    patches = old.patches ++ [
      # Fix OS window role (needed for window managers like yabai)
      (fetchpatch {
        url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-28/fix-window-role.patch";
        sha256 = "+z/KfsBm1lvZTZNiMbxzXQGRTjkCFO4QPlEK35upjsE=";
      })
      # Enable rounded window with no decoration
      (fetchpatch {
        url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-30/round-undecorated-frame.patch";
        sha256 = "uYIxNTyfbprx5mCqMNFVrBcLeo+8e21qmBE3lpcnd+4=";
      })
      # Make Emacs aware of OS-level light/dark mode
      (fetchpatch {
        url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-30/system-appearance.patch";
        sha256 = "3QLq91AQ6E921/W9nfDjdOUWR8YVsqBAT/W9c1woqAw=";
      })
      # Make Tree-sitter compatible with MacOS
      (fetchpatch {
        url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-30/treesit-compatibility.patch";
        sha256 = "zJHcQ604D7D3pCF+hNfbf8p1xW5490yzrMt1lUsyJQY=";
      })
    ];
  });

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
