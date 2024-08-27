{
  services = {
    # Automatically snapshot ZFS volumes
    sanoid = {
      enable = true;

      datasets = {
        "storagepool/backups" = {
          recursive = true;
          daily = 90;
          hourly = 72;
          monthly = 36;
          autosnap = true;
          autoprune = true;
        };
        "storagepool/data" = {
          recursive = true;
          daily = 90;
          hourly = 72;
          monthly = 36;
          autosnap = true;
          autoprune = true;
        };
      };
    };

    # Automatically replicate data pool to onsite backup
    syncoid = {
      localTargetAllow = [
        "change-key"
        "compression"
        "create"
        "destroy"
        "mount"
        "mountpoint"
        "receive"
        "rollback"
      ];
      enable = true;

      commands = {
        "storagepool/data" = {
          target = "backuppool/storagepool/data";
          recursive = true;
        };
        "storagepool/backups" = {
          target = "backuppool/storagepool/backups";
          recursive = true;
        };
      };
    };
  };
}
