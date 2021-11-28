# Set up an account for myself on every machine
# give myself admin access everyone and normalize
# my credentials
{ pkgs, ... }:

{
  sops.secrets.users_gaelan_password = {
    sopsFile = ../../secrets/common.json;
    format = "json";
  };

  users.users.gaelan = {
    shell = pkgs.zsh;
    isNormalUser = true;
    home = "/home/gaelan";
    description = "Gaelan D'costa";
    extraGroups = [ "wheel" ];
    openssh.authorizedKeys.keyFiles = [
     ./gaelan-yubikey.pub
    ];
    passwordFile = "/run/secrets/users_gaelan_password";
  };

  # Trust 'gaelan' account with nix packages
  nix.trustedUsers = [ "gaelan" ];

  security.sudo.extraRules = [
    {
      users = [ "gaelan" ];
      runAs = "root";
      commands = [
        "NOPASSWD:ALL"
      ];
    }
  ];
}
