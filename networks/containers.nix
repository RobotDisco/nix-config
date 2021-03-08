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
    fileSystems."/srv/git" = {
      device = "darktower.admin.robot-disco.net:/salusajail/data/git";
      fsType = "nfs";
    };

    containers.git = {
      bindMounts = {
        "/srv/git" = {
          hostPath = "/srv/git";
          mountPoint = "/srv/git";
          isReadOnly = false;
        };
      };
      config = {config, pkgs, ... }:
        {
          services.postgresql = {
            enable = true;
            dataDir = "/srv/git/db/postgres-data11";
            settings = {
              password_encryption = "scram-sha-256";
            };
            authentication = "local gitea gitea scram-sha-256";
          };

          services.postgresqlBackup = {
            location = "/srv/git/db/backups";
            databases = "gitea";
          };

          services.gitea = {
            rootUrl = "https://git.robot-disco.net/";
            appName = "gitea: A version-controlled discolation of the norm";
            stateDir = "/srv/git/gitea";
            domain = "cloud.robot-disco.net";
            log.rootPath = "/srv/git/gitea/log";
            lfs.contentDir = "/srv/git/lfs";
            dump.backupDir = "/srv/git/gitea/dump";
            cookieSecure = true;
            database.type = "postgres";
            database.socket = "/run/postgresql/";
            database.passwordFile = "/srv/git/gitea/secrets/dbpw";
            database.createDatabase = false;
            repositoryRoot = "/srv/git/repositories";
            enable = true;
            disableRegistration = true;
          };
        };
    };

    networking.nat.enable = true;
    networking.nat.internalInterfaces = ["ve-+"];
    networking.nat.externalInterface = "vlan50";
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
      ../profiles/mta.nix
    ];
  };
}
