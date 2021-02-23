{
  salusa0 = {
    deployment.targetHost = "salusa0.admin.robot-disco.net";
    networking.hostName = "salusa0";
    imports = [
      ../profiles/kvm-guest.nix
      ../profiles/docker.nix
      ../profiles/traefik.nix
    ];

    networking.interfaces.ens3.useDHCP = true;

    networking.vlans = {
      vlan50 = { id = 50; interface = "ens4"; };
    };
    
    networking.interfaces.vlan50.ipv4 = {
      addresses = [ { address = "192.168.50.99"; prefixLength = 24; } ];
    };

    fileSystems."/srv/webdav" = {
      device = "darktower.admin.robot-disco.net:/salusajail/data/webdav";
      fsType = "nfs";
    };
    fileSystems."/srv/bitwarden" = {
      device = "darktower.admin.robot-disco.net:/salusajail/data/bitwarden";
      fsType = "nfs";
    };
  };

  salusa1 = {
    deployment.targetHost = "salusa1.admin.robot-disco.net";
    networking.hostName = "salusa1";
    imports = [
      ../profiles/kvm-guest.nix
      ../profiles/docker.nix
      ../profiles/minecraft.nix
    ];

    networking.interfaces.ens3.useDHCP = true;

    networking.vlans = {
      vlan50 = { id = 50; interface = "ens4"; };
    };

    fileSystems."/srv/minecraft" = {
      device = "darktower.admin.robot-disco.net:/salusajail/data/minecraft";
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
