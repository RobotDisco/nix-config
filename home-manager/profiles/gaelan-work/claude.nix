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
        "atlassian@claude-plugins-official" = true;
        "gitlab@claude-plugins-official" = true;
        "slack@claude-plugins-official" = true;
      };
    };
  };
}
