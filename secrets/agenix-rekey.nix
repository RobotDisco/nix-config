{
  config,
  # osConfig is our NixOS config, when in home-manager
  osConfig ? null,
  pkgs,
  robotdisco-secrets,
  ...
}:

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
      else
        "${osConfig.networking.hostName}-${config.home.username}"
    };

    storageMode = "local";
  };
}
