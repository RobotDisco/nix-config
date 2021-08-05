{ config, pkgs, lib, ... }:

{
    users.users.gaelan = {
      isNormalUser = true;
      home = "/home/gaelan";
      description = "Gaelan D'costa";
      # TODO if I have role profiles elsewhere, these should be confingured there
      extraGroups = [ "wheel" "networkmanager" "docker" "video" ];
    };
}