{
  containers = {
    reverseproxy.config.services.nginx.virtualHosts."feeds.robot-disco.net" = {
      locations."/" = {
        proxyPass = "http://192.168.50.3:80";
      };
      forceSSL = true;
      enableACME = true;
    };

    postgresql.config.services.postgresql = {
      authentication = ''
        host freshrss freshrss 192.168.50.3/32 scram-sha-256
      '';
      ensureDatabases = [ "freshrss" ];
      ensureUsers = [
        {
          name = "freshrss";
          ensureDBOwnership = true;
        }
      ];
    };

    feeds = {
      autoStart = true;
      privateNetwork = true;
      hostBridge = "br50";
      localAddress = "192.168.50.3/24";
      bindMounts = {
        "/var/lib/freshrss" = {
          hostPath = "/srv/storagepool/data/freshrss";
          isReadOnly = false;
        };
      };

      config = {
        system.stateVersion = "26.05";

        networking = {
          defaultGateway = "192.168.50.1";
          firewall.allowedTCPPorts = [ 80 ];
          nameservers = [ "192.168.50.1" ];
        };

        services.freshrss = {
          api.enable = true;
          enable = true;

          baseUrl = "https://feeds.robot-disco.net";
          defaultUser = "gaelan";
          passwordFile = "/var/lib/freshrss/admin_password";
          virtualHost = "feeds.robot-disco.net";

          database = {
            type = "pgsql";
            host = "192.168.10.3";
            passFile = "/var/lib/freshrss/db_password";
          };
        };
      };
    };
  };
}
