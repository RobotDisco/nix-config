{ pkgs, inputs, ... }:

{
  # Enable nix flake support
  nix = {
    package = pkgs.nixUnstable;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };

  # Configure nix overlays. Why flake.nix config didn't take, I do not know.
  nixpkgs = {
    config.allowUnfree = true;
    overlays = [ inputs.emacs-overlay.overlay ];
  };
}
