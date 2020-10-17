{
  users.users.gaelan = {
    extraGroups = [ "libvirtd" ];
  };
  
  virtualisation.libvirtd.enable = true;
}
