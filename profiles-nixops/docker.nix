{ pkgs, ... }:

{
  users.users.gaelan = {
    extraGroups = [ "docker" ];
  };

  virtualisation.docker.enable = true;

  environment.systemPackages = [
    pkgs.docker-compose
  ];
}
