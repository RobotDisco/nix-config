{
  # I gave up on rooting my phone, so no point having virtualbox any more.
  virtualisation.virtualbox.host.enable = false;
  users.extraGroups.vboxusers.members = [ "gaelan" ];
  virtualisation.virtualbox.host.enableExtensionPack = true;

  programs.adb.enable = true;
  users.users.gaelan.extraGroups = ["adbusers"];
}
