{ config, robotdisco-secrets, ... }:

{
  age.secrets.seafile-envs.rekeyFile = "${robotdisco-secrets}/seafile-envs.age";

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

  networking.firewall.interfaces.eno1.allowedTCPPorts = [ 8001 ];

  virtualisation.oci-containers.containers = {
    "seafile-memcached" = {
      autoStart = true;
      image = "memcached:1.6.18";
      entrypoint = "memcached";
      # In theory we can isolate our seafile pods into a seafile network.
      # However, in practice, these don't allow for access to localhost ports
      # TODO Debug this.
      #networks = [ "seafile" ];
      cmd = [
        "-m"
        "256"
      ];
    };
    "seafile-mc" = {
      autoStart = true;
      image = "seafileltd/seafile-mc:11.0-latest";
      dependsOn = [ "seafile-memcached" ];
      environmentFiles = [ config.age.secrets.seafile-envs.path ];
      #networks = [ "seafile" ];
      volumes = [ "/srv/storagepool/data/webdav/shared:/shared" ];
      ports = [ "192.168.10.3:8001:8000" ];
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
