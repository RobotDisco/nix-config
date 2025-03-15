{ pkgs, ... }:

{
  programs.rbw = {
    enable = true;
    settings = {
      base_url = "https://vaultwarden.robot-disco.net/raziel/";
      email = "gdcosta+bitwarden@gmail.com";
      pinentry = if pkgs.stdenv.isDarwin then pkgs.pinentry_mac else pkgs.pinentry-gtk2;
      ui_url = "https://vaultwarden.robot-disco.net/raziel/";
    };
  };
}
