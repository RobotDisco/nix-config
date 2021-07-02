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

    networking.bridges.br-container = {
      interfaces = [];
      rstp = true;
    };
    networking.interfaces.br-container.ipv4 = {
      addresses = [
        { address = "192.168.51.1"; prefixLength = 24; }
        { address = "192.168.52.1"; prefixLength = 24; }
      ];
    };

    networking.nat.enable = true;
    networking.nat.internalInterfaces = ["ve-+" "vb-+" "br-container" ];
    networking.nat.externalInterface = "vlan50";

    fileSystems."/srv/webdav" = {
      device = "darktower.admin.robot-disco.net:/salusajail/data/webdav";
      fsType = "nfs";
    };
    fileSystems."/srv/bitwarden" = {
      device = "darktower.admin.robot-disco.net:/salusajail/data/bitwarden";
      fsType = "nfs";
    };
    fileSystems."/srv/nfs" = {
      device = "darktower.admin.robot-disco.net:/salusajail/data/salusa0.admin.robot-disco.net";
      fsType = "nfs";
    };
    fileSystems."/srv/nfs/postgres" = {
      device = "darktower.admin.robot-disco.net:/salusajail/data/salusa0.admin.robot-disco.net/postgres";
      fsType = "nfs";
    };
    fileSystems."/srv/nfs/git" = {
      device = "darktower.admin.robot-disco.net:/salusajail/data/salusa0.admin.robot-disco.net/git";
      fsType = "nfs";
    };

    networking.firewall.allowedTCPPorts = [ 2222 ];

    containers.postgres = {
      bindMounts = {
        "/srv/db" = {
          hostPath = "/srv/nfs/postgres";
          mountPoint = "/srv/db";
          isReadOnly = false;
        };
      };
      config = {config, pkgs, ... }:
        {
          services.postgresql = {
            package = pkgs.postgresql_12;
            dataDir = "/srv/db/data-12";
            enable = true;
            enableTCPIP = true;
            settings = {
              password_encryption = "scram-sha-256";
            };
            authentication = ''
host bitwarden bitwarden 192.168.51.3/32 scram-sha-256
host gitea gitea 192.168.51.4/32 scram-sha-256
'';
          };

          services.postgresqlBackup = {
            enable = false;
            location = "/srv/db/backup";
            databases = [ "gitea" "bitwarden" ];
          };

          networking.defaultGateway = "192.168.51.1";
          networking.nameservers = [ "192.168.50.1" ];

          networking.firewall.allowedTCPPorts = [ 5432 ];
        };

      privateNetwork = true;
      hostBridge = "br-container";
      localAddress = "192.168.51.2/24";
    };

    #containers.bitwarden = {
    #  privateNetwork = true;
    #  hostBridge = "br-container";
    #  localAddress = "192.168.51.3/24";

    #  config = {config, pkgs, ... }:
    #    {
    #      networking.defaultGateway = "192.168.51.1";
    #      networking.nameservers = [ "192.168.50.1" ];
    #            
    #      services.bitwarden_rs = {
    #        enable = false;
    #        dbBackend = "postgresql";
    #        # backupDir = "/srv/bitwarden-backup";
    #        config = {
    #          databaseUrl = "postgresql://bitwarden:Gvq59mUprUKWGKAzi6vM7mYMco8iJ68P@192.168.51.2/bitwarden";
    #          adminToken = "HQV8RfNmD97H2QMnb6jAFAWWgntFy6QGzYe8E6QBUxSpa994";
    #          domain = "http://bitwarden2.robot-disco.net";
    #          yubicoClientId = "63777";
    #          yubicoSecretKey= "kMLBIhbd58gd3wpiR9Y3Y/IepT0=";
    #        };
    #      };

    #       networking.firewall.allowedTCPPorts = [ 8000 ];
    #    };
    #};

    containers.homepage = {
      privateNetwork = true;
      hostBridge = "br-container";
      localAddress = "192.168.52.2/24";

      forwardPorts = [
        {
         hostPort = 2223;
         containerPort = 22;
         protocol = "tcp";
       }
     ];
      
      config = {config, pkgs, ... }:
        {
          networking.defaultGateway = "192.168.52.1";
          networking.nameservers = [ "192.168.50.1" ];

          services.nginx = {
            enable = true;
            
            virtualHosts."www.robot-disco.net".root = "/srv/www.robot-disco.net/public";
          };

          networking.firewall.allowedTCPPorts = [ 80 ];

          services.openssh = {
            enable = true;
          };
        };
    };
    
    containers.git = {
     bindMounts = {
       "/srv/git" = {
         hostPath = "/srv/nfs/git";
         mountPoint = "/srv/git";
         isReadOnly = false;
       };
     };

     forwardPorts = [
       {
         hostPort = 2222;
         containerPort = 2222;
         protocol = "tcp";
       }
     ];
     
     config = {config, pkgs, ... }:
       {
         services.gitea = {
           rootUrl = "https://git.robot-disco.net/";
           appName = "gitea: A version-controlled discolation of the norm";
           stateDir = "/srv/git/gitea";
           domain = "cloud.robot-disco.net";
           log.rootPath = "/srv/git/log";
           lfs.contentDir = "/srv/git/lfs";
           dump.backupDir = "/srv/git/dump";
           dump.enable = true;
           cookieSecure = true;
           database.type = "postgres";
           database.port = 5432;
           database.host = "192.168.51.2";
           database.passwordFile = "/srv/git/secrets/dbpw";
           database.createDatabase = false;
           repositoryRoot = "/srv/git/repositories";
           enable = true;
           disableRegistration = true;
           httpAddress = "192.168.51.4";
           ssh.clonePort = 2222;
         };

         services.openssh = {
           enable = true;
           ports = [ 2222 ];
           allowSFTP = false;
           permitRootLogin = "no";
           passwordAuthentication = false;
         };
    
         #services.sshguard.enable = true;

         networking.defaultGateway = "192.168.51.1";
         networking.nameservers = [ "192.168.50.1" ];

         networking.firewall.allowedTCPPorts = [ 3000 ];
       };

     privateNetwork = true;
     hostBridge = "br-container";
     localAddress = "192.168.51.4/24";
    };

    virtualisation.oci-containers.containers.drone-server = {
      image = "drone/drone:1";
      ports = [
        "8300:443"
      ];
      extraOptions = [
        "--label=traefik.enable=true"
        "--label=traefik.http.routers.drone.rule=Host(`ci.robot-disco.net`)"
        "--label=traefik.http.routers.drone.entrypoints=websecure"
        "--label=traefik.http.routers.drone.tls=true"
        "--label=traefik.http.routers.drone.tls.certresolver=letsencrypt"
      ];
      environment = {
        DRONE_GITEA_CLIENT_ID = "0c7c7d24-5dcf-40af-94b7-8f5cf520c190";
        DRONE_GITEA_CLIENT_SECRET = "_tlrcrW_Ds7V4BUtxoSddq0r22TvVRDCTdYpeQWRLqE=";
        DRONE_GITEA_SERVER = "https://git.robot-disco.net";
        DRONE_RPC_SECRET = "uejc2sino4abg4cx";
        DRONE_SERVER_HOST = "ci.robot-disco.net";
        DRONE_SERVER_PROTO = "https";
      };
    };
    virtualisation.oci-containers.containers.drone-runner = {
      image = "drone/drone-runner-docker:1";
      environment = {
        DRONE_RPC_PROTO = "https";
        DRONE_RPC_HOST = "ci.robot-disco.net";
        DRONE_RPC_SECRET = "uejc2sino4abg4cx";
        DRONE_RUNNER_CAPACITY = "2";
        DRONE_RUNNER_NAME="salusa0";
      };
      ports = [
        "8301:3000"
      ];
      volumes = [
        "/var/run/docker.sock:/var/run/docker.sock"
      ];
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
    
    networking.bridges.br-container = {
      interfaces = [ "vlan50" ];
      rstp = true;
    };
    networking.interfaces.br-container.ipv4 = {
      addresses = [ { address = "192.168.50.100"; prefixLength = 24; } ];
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
