{ pkgs, inputs, ... }:

{
  # Configure nix overlays. Why flake.nix config didn't take, I do not know.
  nixpkgs = {
    overlays = [ inputs.emacs-overlay.overlay ];
  };
}
