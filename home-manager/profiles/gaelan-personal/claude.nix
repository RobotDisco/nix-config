{ pkgs, nixpkgs-unstable, ... }:

let
  pkgs-unstable = import nixpkgs-unstable {
    inherit (pkgs) system;
    config.allowUnfree = true;
  };
in

{
  programs.claude-code = {
    enable = true;
    package = pkgs-unstable.claude-code;

    settings = {
      enabledPlugins = {
        "slack@claude-plugins-official" = true;
      };
    };
  };
}
