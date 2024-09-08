{
  lib,
  pkgs,
  robotdisco-secrets,
  ...
}:

let
  s = robotdisco-secrets;
in
lib.mkMerge [
  (lib.mkIf pkgs.stdenv.isDarwin {
    age = {
      # Because we're relying on a subshell expression to resolve part of this
      # string, explicitly specify the command as the path seems to not be
      # set otherwise.
      secretsDir = "$(/usr/bin/getconf DARWIN_USER_TEMP_DIR)/agenix";
      secretsMountPoint = "$(/usr/bin/getconf DARWIN_USER_TEMP_DIR)/agenix.d";
    };
  })
  {
    age = {
      # We need to ensure age-plugin-yubikey is in age's path.
      ageBin = "PATH=$PATH:${lib.makeBinPath [ pkgs.age-plugin-yubikey ]} ${pkgs.age}/bin/age";

      # Include identityPaths for Yubikeys
      identityPaths = [
        "${s}/yk-main.id"
        "${s}/yk-work.id"
        "${s}/yk-backup.id"
      ];

      secrets =
        # let
        ## Haven't used common schemes for files yet, worry about later.
        # user_readable = {
        #   mode = "0400";
        #   owner = "gaelan";
        # };
        # in
        {
          okta-yaml = {
            file = "${s}/okta-aws-cli.yaml.age";
          };
          u2f_keys = {
            file = "${s}/u2f_keys.age";
          };
        };
    };
  }
]
