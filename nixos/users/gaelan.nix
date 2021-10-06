# Set up an account for myself on every machine
# give myself admin access everyone and normalize
# my credentials
{ pkgs, ... }:

{
  users.users.gaelan = {
    shell = pkgs.zsh;
    isNormalUser = true;
    home = "/home/gaelan";
    description = "Gaelan D'costa";
    extraGroups = [ "wheel" ];
    openssh.authorizedKeys.keyFiles = [
     ./gaelan-yubikey.pub
    ];
  };

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
