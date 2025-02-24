{ pkgs, ... }:
let
  username = "gaelan";
in
{
  robot-disco = {
    development-environment = {
      enable = true;

      fullname = "Gaelan D'costa";
      email = "gaelan@tulip.com";
      gpgKey = "0x00729AD1F1840227!";

      signCommits = true;
    };

    emacs = {
      enable = true;
      package = pkgs.gaelan-emacs-macport;
    };

    gnupg.enable = true;

    tulip.enable = true;
  };

  home = {
    inherit username;
    homeDirectory = "/Users/${username}";
    # The state version is required and should stay at the version you
    # originally installed.
    stateVersion = "22.11";
  };

  programs.zsh.enable = true;
}
