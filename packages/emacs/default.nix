{ pkgs }:

let
  runtime = import ./runtime.nix { inherit pkgs; };
in
{
  gaelan-emacs = runtime;
  gaelan-emacs-config = (pkgs.emacsPackagesFor runtime.emacs).callPackage
    ./config.nix {
      packageRequires = runtime.explicitRequires;
    };
}
