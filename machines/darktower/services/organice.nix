{
  containers.reverseproxy.config.services.nginx.virtualHosts."organice.robot-disco.net" = {
    locations."/" = {
      proxyPass = "http://localhost:8002";
    };

    forceSSL = true;
    enableACME = true;
  };

  virtualisation.oci-containers.containers = {
    "organice" = {
      autoStart = true;
      image = "twohundredok/organice:latest";
      ports = [ "127.0.0.1:8002:5000" ];
      environmentFiles = [ "/srv/storagepool/data/webdav/organice_env_vars" ];
    };
  };
}
