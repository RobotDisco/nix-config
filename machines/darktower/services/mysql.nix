{ pkgs, ... }:

{
  containers = {
    mysql = {
      autoStart = true;
      bindMounts = {
        "/var/backup/mysql" = {
          hostPath = "/srv/storagepool/backups/mariadb";
          isReadOnly = false;
        };
      };
      config = {
        system.stateVersion = "21.05";
        services.mysql = {
          enable = true;
          ensureDatabases = [
            "ccnet_db"
            "seafile_db"
            "seahub_db"
          ];
          package = pkgs.mariadb;
        };
        services.mysqlBackup = {
          enable = true;
          databases = [
            "ccnet_db"
            "seafile_db"
            "seahub_db"
          ];
          calendar = "*-*-* *:00,15,30,45:00";
          location = "/var/backup/mysql";
        };
      };
    };
  };

  networking.firewall.interfaces.podman0.allowedTCPPorts = [
    3306
  ];
}
