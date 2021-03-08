{
  services.traefik = {
    enable = true;

    staticConfigOptions = {
      accessLog = {};

      #defaultEntryPoints = ["web" "websecure"];

      entryPoints.admin = {
        address = ":8080";
      };
      
      entryPoints.web = {
        address = ":80";

        http.redirections.entryPoint = {
          to = "websecure";
          scheme = "https";
        };
      };

      entryPoints.websecure = {
        address = ":443";
      };

      certificatesResolvers.letsencrypt.acme = {
        email = "gdcosta+letsencrypt@gmail.com";
        storage = "/var/cache/acme.json";

        tlsChallenge = {};

        #caServer = "https://acme-staging-v02.api.letsencrypt.org/directory";
      };

      providers.docker = {
        endpoint = "unix:///var/run/docker.sock";
        exposedByDefault = false;
      };

      api = {
        dashboard = true;
      };
    };

    dynamicConfigOptions = {
      http.routers = {
        dashapi = {
          service = "api@internal";
          rule = "Host(`192.168.50.99`) && (PathPrefix(`/api`) || PathPrefix(`/dashboard`))";
        };
        gitea = {
          entryPoints = ["websecure"];
          rule = "Host(`git.robot-disco.net`)";
          service = "gitea@file";
          tls.certResolver = "letsencrypt";
        };
      };

      http.services = {
        gitea = {
          loadBalancer = {
            servers = [{
              url = "http://localhost:3000";
            }];
          };
        };
      };
    };

    group = "docker";
  };

  networking.firewall.checkReversePath = "loose";
  networking.firewall.allowedTCPPorts = [ 8080 80 443 ];
}
