{
  config,
  # osConfig is our NixOS config, when in home-manager
  osConfig ? null,
  # hostName fallback for standalone home-manager (no osConfig available)
  hostName ? null,
  lib,
  pkgs,
  robotdisco-secrets,
  ...
}:

lib.mkMerge [
  (lib.mkIf pkgs.stdenv.isDarwin {
    age = {
      secretsDir = "$(/usr/bin/getconf DARWIN_USER_TEMP_DIR)/agenix";
      secretsMountPoint = "$(/usr/bin/getconf DARWIN_USER_TEMP_DIR)/agenix.d";
    };
  })
  {
    age.rekey = {

      agePlugins = [
        pkgs.age-plugin-yubikey
      ];

      masterIdentities = [
        "${robotdisco-secrets}/main/yk-main.pub"
        "${robotdisco-secrets}/main/yk-backup.pub"
      ];

      localStorageDir = ../secrets/rekeyed/${
        if (config ? networking.hostName) then
          config.networking.hostName
        else if osConfig != null then
          "${osConfig.networking.hostName}-${config.home.username}"
        else
          "${hostName}-${config.home.username}"
      };

      storageMode = "local";
    };
  }
]
