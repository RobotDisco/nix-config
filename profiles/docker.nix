{
  users.users.gaelan = {
    extraGroups = [ "docker" ];
  };

  virtualisation.docker.enable = true;

  services.traefik.group = "docker";
}
