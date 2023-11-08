{
  virtualisation.virtualbox.host.enable = true;
  users.extraGroups.vboxusers.members = [ "gaelan" ];
  virtualisation.virtualbox.host.enableExtensionPack = true;

  programs.adb.enable = true;
  users.users.gaelan.extraGroups = ["adbusers"];
}
