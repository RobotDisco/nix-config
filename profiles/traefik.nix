{
  services.traefik = {
    enable = true;

    configOptions = {
      debug = true;
      
      accessLog = {};
      traefikLog = {};

      defaultEntryPoints = ["http" "https"];
      
      entryPoints = {
        http = {
          address = ":80";
          compress = true;
          redirect = {
            entryPoint = "https";
          };
        };
        https = {
          address = ":443";
          compress = true;
          tls = {};
        };
          
        dashboard = {
          address = ":8080";
          compress = true;
        };
      };

      acme = {
        email = "gdcosta+letsencrypt@gmail.com";
        storage = "/var/cache/acme.json";
        entryPoint = "https";

        tlsChallenge = {};

        #caServer = "https://acme-staging-v02.api.letsencrypt.org/directory";

        domains = [
          {
            main = "fallcube.robot-disco.net";
            sans = [ "bitwarden.robot-disco.net" ];
          }
        ]; 
      };

      ping = {
        entrypoint = "dashboard";
      };
      
      api = {
        entrypoint = "dashboard";
        dashboard = true;
        debug = true;
      };

      docker = {
        endpoint = "unix:///var/run/docker.sock";
        watch = true;
        exposedByDefault = false;
        domain = "robot-disco.net";
      };
    };
  };

  networking.firewall.allowedTCPPorts = [ 80 443 ];
}
