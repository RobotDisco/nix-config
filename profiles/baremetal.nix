{
  services.fwupd.enable = true;

  services.smartd = {
    enable = true;
    # TODO make log directory in nix somehow
    extraOptions = [
      "-A /var/log/smartd/"
      "-i 3600"
    ];
    notifications = {
      test = true;
      mail.sender = "root@robot-disco.net";
      mail.enable = true;
      mail.recipient = "gdcosta@gmail.com";
    };
  };

  # Scrub all ZFS pools monthly
  services.zfs.autoScrub = {
    interval = "monthly";
    enable = true;
  };
}
