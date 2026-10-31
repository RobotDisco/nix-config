{ config, robotdisco-secrets, ... }:

{
  age.secrets.seafile-envs.file = "${robotdisco-secrets}/seafile-envs.age";

  # Needed while I keep stuff in the default podman network
  virtualisation.podman.defaultNetwork.settings.dns_enabled = true;

  containers.reverseproxy.config.services.nginx.virtualHosts."fallcube.robot-disco.net" = {
    extraConfig = ''
      proxy_set_header X-Forwarded-For $remote_addr;
    '';

    locations."/" = {
      proxyPass = "http://192.168.10.3:8001";

      extraConfig = ''
        client_max_body_size 0;
      '';
    };

    forceSSL = true;
    enableACME = true;
  };

  virtualisation.oci-containers.containers = {
    "seafile-redis" = {
      autoStart = true;
      image = "redis";
      cmd = [
        "/bin/sh"
        "-c"
        "exec redis-server --requirepass \"$REDIS_PASSWORD\" --save \"\" --appendonly no"
      ];
      environmentFiles = [ config.age.secrets.seafile-envs.path ];
      #networks = [ "seafile" ];
    };
    "seafile" = {
      autoStart = true;
      image = "seafileltd/seafile-mc:13.0-latest";
      dependsOn = [
        "seafile-redis"
      ];
      environmentFiles = [ config.age.secrets.seafile-envs.path ];
      #networks = [ "seafile" ];
      volumes = [ "/srv/storagepool/data/webdav/shared:/shared" ];
      ports = [ "192.168.10.3:8001:80" ];
      extraOptions = [
        "--health-cmd=curl -f http://localhost:80 || exit 1"
        "--health-interval=30s"
        "--health-timeout=10s"
        "--health-retries=3"
        "--health-start-period=10s"
      ];
    };
    "seafile-thumbnail" = {
      autoStart = true;
      image = "seafileltd/thumbnail-server:13.0-latest";
      dependsOn = [
        "seafile"
      ];
      volumes = [ "/srv/storagepool/data/webdav/shared:/shared" ];
      environmentFiles = [ config.age.secrets.seafile-envs.path ];
      #networks = [ "seafile" ];
    };
  };
}
