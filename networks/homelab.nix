{
  darktower = {
    deployment.targetHost = "192.168.10.3";
    networking.hostName = "darktower.admin.robot-disco.net";
    imports = [
      ../machines/darktower/hardware-configuration.nix
      ../profiles/hypervisor.nix
      ../profiles/baremetal.nix
    ];
  };
  
  network.description = "Gaelan Homelab ISP";

  defaults = {
    imports = [
      ../profiles/baseline.nix
      ../profiles/ssh.nix
      ../profiles/users.nix
    ];
  };
}  
