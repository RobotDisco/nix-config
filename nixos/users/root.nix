{ pkgs, ... }:

{
  sops.secrets.users_root_password = {
    sopsFile = ../../secrets/common.json;
    format = "json";
  };

  users.users.root = {
    passwordFile = "/run/secrets/users_root_password";
  };
}
