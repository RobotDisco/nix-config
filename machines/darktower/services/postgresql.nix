{ pkgs, ... }:

{
  containers = {
    postgresql = {
      autoStart = true;
      bindMounts = {
        "/var/backup/postgresql" = {
          hostPath = "/srv/storagepool/backups/postgresql";
          isReadOnly = false;
        };
      };
      config = {
        system.stateVersion = "21.05";
        services.postgresql = {
          package = pkgs.postgresql_17;
          enable = true;
          enableTCPIP = false;
          settings.password_encryption = "scram-sha-256";
        };
        services.postgresqlBackup = {
          enable = true;
          location = "/var/backup/postgresql";
          startAt = "*-*-* *:00,15,30,45:00";
        };
        # For upgrading
        # see https://nixos.org/manual/nixos/stable/index.html#module-services-postgres-upgrading
        environment.systemPackages = [
          # pkgs.postgresql_18
        ];
      };
    };
  };
}
