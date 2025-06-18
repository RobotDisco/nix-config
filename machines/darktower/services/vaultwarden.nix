{
  containers = {
    postgresql.config.services.postgresql = {
      authentication = ''
        host vaultwarden vaultwarden 192.168.50.2/32 scram-sha-256
      '';
      ensureDatabases = [ "vaultwarden" ];
    };

    reverseproxy.config.services.nginx.virtualHosts."vaultwarden.robot-disco.net" = {
      locations."/raziel/" = {
        proxyPass = "http://192.168.50.2:8000";
      };
      forceSSL = true;
      enableACME = true;
    };

    vaultwarden = {
      autoStart = true;
      privateNetwork = true;
      hostBridge = "br50";
      localAddress = "192.168.50.2/24";
      bindMounts = {
        "/var/lib/bitwarden_rs" = {
          hostPath = "/srv/storagepool/data/vaultwarden";
          isReadOnly = false;
        };
      };
      config = {
        system.stateVersion = "21.05";

        networking = {
          defaultGateway = "192.168.50.1";
          firewall.allowedTCPPorts = [ 8000 ];
          nameservers = [ "192.168.50.1" ];
        };

        environment.etc = {
          "fail2ban/filter.d/vaultwarden.conf".text = ''
            [INCLUDES]
            before = common.conf

            [Definition]
            failregex = ^.*Username or password is incorrect\. Try again\. IP: <ADDR>\. Username:.*$
            ignoreregex =
            journalmatch = UNIT=vaultwarden.service
          '';
          "fail2ban/filter.d/vaultwarden-admin.conf".text = ''
            [INCLUDES]
            before = common.conf

            [Definition]
            failregex = ^.*Invalid admin token\. IP: <ADDR>.*$
            ignoreregex =
            journalmatch = UNIT=vaultwarden.service
          '';
        };

        services = {
          fail2ban = {
            banaction-allports = "iptables-allports";
            enable = true;
            ignoreIP = [
              "192.168.0.0/16"
            ];
            jails = {
              vaultwarden-webvault = ''
                enabled = true
                port = 8000
                filter = vaultwarden
                banaction = %(banaction_allports)s
                backend = systemd
                maxretry = 3
                bantime = 14400
                findtime = 14400
              '';
              vaultwarden-admin = ''
                enabled = true
                port = 8000
                filter = vaultwarden-admin
                banaction = %(banaction_allports)s
                backend = systemd
                maxretry = 3
                bantime = 14400
                findtime = 14400
              '';
            };
          };

          vaultwarden = {
            enable = true;
            dbBackend = "postgresql";
            environmentFile = "/var/lib/bitwarden_rs/vaultwarden_secrets";
            config = {
              rocket_address = "192.168.50.2";

              signups_allowed = false;
              signups_verify = true;
              show_password_hint = false;

              domain = "https://vaultwarden.robot-disco.net/raziel";
              invitation_org_name = "Robot Disco";

              smtp_host = "smtp.teksavvy.com";
              smtp_from = "gdcosta@gmail.com";
              smtp_from_name = "Vaultwarden";

              require_device_email = false;
            };
          };
        };
      };
    };
  };
}
