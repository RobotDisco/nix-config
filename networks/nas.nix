{
  chapterhouse = {
    deployment.targetHost = "192.168.10.4";
    networking.hostName = "chapterhouse.admin.robot-disco.net";
    imports = [
      ../machines/chapterhouse/hardware-configuration.nix
    ];
  };
  
  network.description = "Gaelan Homelab SAN";

  defaults = {
    imports = [
      ../profiles/baseline.nix
      ../profiles/ssh.nix
      ../profiles/users.nix
    ];
  };
}
