{
  services.nfs.server.enable = true;
  services.nfs.server.exports = ''
    /salusajail/data/webdav salusa?.admin.robot-disco.net(rw,no_root_squash)
    /salusajail/data/bitwarden salusa?.admin.robot-disco.net(rw,no_root_squash)
    /salusajail/data/minecraft salusa?.admin.robot-disco.net(rw)
    /salusajail/data/git salusa?.admin.robot-disco.net(rw)
  '';

  services.samba = {
    enable = true;
    syncPasswordsByPam = true;

    extraConfig = ''
     workgroup=ROBOT-DISCO
     server string = fileserver
     netbios name = fileserver
#     hosts allow = 192.168.20 localhost
#     hosts deny = 0.0.0.0/0
     guest account = nobody
     map to guest = bad user
    '';
    
    shares = {
      fileserver = {
        path = "/salusajail/data/fileserver";
        browseable = "yes";
        comment = "Gaelan's fileserver";
        "read only" = "no";
        "create mask" = "0644";
        "directory mask" = "0755";
        "force user" = "gaelan";
        "force group" = "users";
      };
    };
  };
  networking.firewall.allowedTCPPorts = [ 445 139 ];
  networking.firewall.allowedUDPPorts = [ 137 138 ];
}
