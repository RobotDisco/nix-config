/*
  Emacs Package Override Configuration

  This file exists to solve a common problem in the Nix ecosystem: community
  overlays sometimes break or build packages from source when we want binaries.

  ## What are overlays?
  In Nix, "overlays" are functions that modify the package set (nixpkgs). They
  let you override package definitions, add new packages, or change how existing
  packages are built. Think of them as "patches" to the package collection.

  ## Why do we need overrides?
  The emacs-overlay (from nix-community) is incredibly useful - it provides
  the latest Emacs packages and builds. However, it sometimes:
  1. Builds packages from source instead of using pre-built binaries
  2. Has breaking changes that don't work with our configuration
  3. Includes experimental packages that aren't stable yet

  ## How this works:
  This file returns a set of package overrides. When a package in emacs-overlay
  is broken or problematic, we add it here to use the stable nixpkgs version
  instead. This overlay gets applied AFTER emacs-overlay, so our definitions
  "win" and override theirs.

  ## When to use this:
  - Add packages here when emacs-overlay breaks them
  - Remove packages from here when emacs-overlay fixes them
  - Document WHY each override exists so future-you remembers

  ## Example workflow:
  1. emacs-overlay updates and breaks tree-sitter-grammars
  2. Add: tree-sitter-grammars = nixpkgs.tree-sitter-grammars;
  3. Your system builds again with working packages
  4. Later, emacs-overlay fixes the issue
  5. Remove the override line - emacs-overlay takes over again
*/

{ nixpkgs, nixpkgs-unstable }:

# Function that returns overrides for a specific system
system: {
  # TEMPLATE: Add more overrides here when emacs-overlay breaks packages
  #
  # Examples:
  # tree-sitter-grammars = nixpkgs-unstable.tree-sitter-grammars;  # Use unstable if needed
  # mu4e = nixpkgs.mu4e;                                           # Use stable version
  #
  # Remember to:
  # 1. Add a comment explaining WHY the override is needed
  # 2. Remove the override when emacs-overlay is fixed
  # 3. Test that your emacs config still works after changes
}
