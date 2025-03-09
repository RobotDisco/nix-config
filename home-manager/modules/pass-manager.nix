{ pkgs, ... }:

{
  programs.rbw = {
    enable = true;
    settings = {
      base_url = "https://vaultwarden.robot-disco.net/raziel/";
      email = "gdcosta+bitwarden@gmail.com";
      pinentry = pkgs.pinentry-gtk2;
      ui_url = "https://vaultwarden.robot-disco.net/raziel/";
    };
  };
}
