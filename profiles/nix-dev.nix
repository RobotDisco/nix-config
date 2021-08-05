{ pkgs, ... }:

{
  environment.systemPackages = [
    pkgs.any-nix-shell
  ];
}

# TODO get rnix in here for lsp goodness
