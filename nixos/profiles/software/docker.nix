# TODO this should be role-based rather than software-based
{
  virtualisation.docker.enable = true;

  users.users.gaelan = {
    extraGroups = [ "wheel" ];
  };
}
