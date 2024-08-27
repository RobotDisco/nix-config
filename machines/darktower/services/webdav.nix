{ config, ... }:

{
  containers.reverseproxy.config.services.nginx.virtualHosts."fallcube.robot-disco.net" = {
    extraConfig = ''
      proxy_set_header X-Forwarded-For $remote_addr;
    '';

    locations."/" = {
      proxyPass = "http://localhost:8001";

      extraConfig = ''
        client_max_body_size 0;
      '';
    };

    forceSSL = true;
    enableACME = true;
  };

  virtualisation.oci-containers.containers = {
    "seafile-memcached" = {
      autoStart = true;
      image = "memcached:1.6.18";
      entrypoint = "memcached";
      cmd = [
        "-m"
        "256"
      ];
      ports = [ "127.0.0.1:11211:11211" ];
    };
    "seafile-mc" = {
      autoStart = true;
      image = "seafileltd/seafile-mc:11.0-latest";
      dependsOn = [ "seafile-memcached" ];
      environmentFiles = [ config.age.secrets.seafile-envs.path ];
      volumes = [ "/srv/storagepool/data/webdav/shared:/shared" ];
      ports = [ "127.0.0.1:8001:8000" ];
    };
  };

  environment.etc = {
    "fail2ban/filter.d/seafile-auth.conf".text = ''
      [INCLUDES]
      before = common.conf

      [Definition]

      _daemon = seaf-server
      failregex = Login attempt limit reached.*, ip: <HOST>
      ignoreregex =
    '';
  };

  services.fail2ban = {
    banaction-allports = "iptables-allports";
    enable = true;
    ignoreIP = [
      "192.168.0.0/16"
    ];
    jails = {
      seafile = ''
        enabled = true
        port = http, https
        filter = seafile-auth
        logpath = /srv/storagepool/data/webdav/shared/seafile/logs/seahub.log
        banaction = %(banaction_allports)s
        maxretry = 3
        bantime = 14400
        findtime = 14400
      '';
    };
  };
}
