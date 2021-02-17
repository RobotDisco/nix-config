{
  salusa0 = {
    deployment.targetHost = "salusa0.admin.robot-disco.net";
    networking.hostName = "salusa0";
    imports = [
      ../profiles/kvm-guest.nix
      ../profiles/docker.nix
      ../profiles/traefik.nix
      ../profiles/minecraft.nix
    ];

    fileSystems."/srv/webdav" = {
      device = "darktower.admin.robot-disco.net:/salusajail/data/webdav";
      fsType = "nfs";
    };
    fileSystems."/srv/bitwarden" = {
      device = "darktower.admin.robot-disco.net:/salusajail/data/bitwarden";
      fsType = "nfs";
    };
  };
  
  network.description = "Gaelan Homelab Compute";

  defaults = {
    imports = [
      ../profiles/baseline.nix
      ../profiles/ssh.nix
      ../profiles/users.nix
    ];
  };
}
