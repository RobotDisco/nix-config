{ lib, nixpkgs }:

let
  supportedSystems = [ "aarch64-darwin" "x86_64-linux" ];

  # For a given system, return the appropate legacyPackages for the given
  # operating system.

  # Note 1: This is done because every "import" of nixpkgs creates a new thunk,
  # which can cause multiple evaluations of nixpkgs
  # see https://discourse.nixos.org/t/using-nixpkgs-legacypackages-system-vs-import/17462 for why.

  # Note 2: Technically, there are different nixpkgs channels for nixos vs.
  # darwin, which track platform-specific test suites at different speeds.
  # Reason: https://discourse.nixos.org/t/on-niv-running-on-mac-which-branch-should-i-use-to-update-to-21-11-i-cant-find-release-21-11-darwin-branch-on-nixpkgs/16446
  # Unfortunately, the nix expression we use to determine the platform is
  # in nixpkgs itself, so we would have to assume assume that the "standard"
  # nixpkgs can be used in MacOS to make this decision.

  # Note 3: This caused the both nixpkgs to be completely evaluated, and thus wound up not being worth it.
  # If I really want to use two different channels, I can't use a shared function, sadly.
  pkgsForSystem = system: nixpkgs.legacyPackages."${system}";

  # For all supported systems
  # given a function that takes a system string as its only input
  # return an attrset where the key is each supported system string and the
  # value is the respective result of calling our supplied function for each
  # system string.
in func:
lib.genAttrs supportedSystems
# This tripped me up the first time I saw it for a while.
# This is basically leveraging a closure to reference a function passed
# in our outer function argument list to be invoked by our inner function
# Understanding what lib.genAttrs does will be important; an attrSet is
# basically Nix's implementation of a map/dictionary.
(system: func (pkgsForSystem system))
