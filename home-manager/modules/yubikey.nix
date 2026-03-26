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
    age.secrets.u2f_keys.rekeyFile = "${robotdisco-secrets}/u2f_keys.age";

    # In home-manager, the .path attribute relies on the nix config
    # eventually resolving environment variables and command subshells.
    #
    # Apparently using mkOutOfStoreSymlink doesn't work, because it doesn't
    # evaluate the .path attribute including resolutions.
    #
    # So instead, use an activation hook.
    home.activation.linkU2fKeys = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      run ln -sf $VERBOSE_ARG "${config.age.secrets.u2f_keys.path}" "${config.home.homeDirectory}/.config/Yubico/u2f_keys"
    '';

    home.packages = lib.mkIf cfg.guiSupport [ pkgs.yubioath-flutter ];
  };
}
