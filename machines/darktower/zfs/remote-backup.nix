{
  services.borgbackup.jobs."borgbase" = {
    paths = [
      "/srv/storagepool/data"
      "/srv/storagepool/backups"
    ];
    repo = "mwhkrvt4@mwhkrvt4.repo.borgbase.com:repo";
    encryption = {
      mode = "keyfile-blake2";
      # TODO encode this securely to not manual file placement
      passCommand = "cat /root/keys/borg_encryption_passphrase";
    };
    compression = "zstd";
    startAt = "*-*-* *:05,35:00";
    prune.keep = {
      hourly = 72;
      daily = 90;
      monthly = 36;
    };
  };
}
