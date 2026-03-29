{
  config,
  lib,
  pkgs,
  robotdisco-secrets,
  ...
}:

let
  cfg = config.robot-disco.yubikey;
in
{
  options.robot-disco.yubikey = {
    enable = lib.mkEnableOption "Yubikey support";

    guiSupport = lib.mkEnableOption "Install yubioath-flutter GUI app for TOTP codes";
  };

  config = lib.mkIf cfg.enable {
    age.secrets.u2f_keys = {
      rekeyFile = "${robotdisco-secrets}/u2f_keys.age";
      path = "${config.home.homeDirectory}/.config/Yubico/u2f_keys";
    };

    home.packages = lib.mkIf cfg.guiSupport [ pkgs.yubioath-flutter ];
  };
}
