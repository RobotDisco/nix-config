{ config, ... }:

{
  sops.secrets.borg_encryption_passphrase = {
    sopsFile = ../../secrets/borg-chapterhouse.json;
    format = "json";
  };

  services.borgbackup.jobs."borgbase" = {
    paths = [
      "/srv/storagepool/data/postgresql"
      "/srv/storagepool/data/vaultwarden"
      "/srv/storagepool/backups"
    ];
    repo = "mwhkrvt4@mwhkrvt4.repo.borgbase.com:repo";
    encryption = {
      mode = "keyfile-blake2";
      passCommand = "cat ${config.sops.secrets.borg_encryption_passphrase.path}";
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
