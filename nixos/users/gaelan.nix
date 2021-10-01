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
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJOz9up91JWgD0QeCr4ub4C+a8w0SgFfdh/NE743B1aF gaelan@arrakis"
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
