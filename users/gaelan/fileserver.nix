{
  fileSystems."/home/gaelan/fileserver" = {
    device = "//192.168.50.99/archive";
    fsType = "cifs";
    options = [
      # Prevent hanging on network split
      "x-systemd.automount"
      "noauto"
      "x-systemd.idle-timeout=60"
      "x-systemd.device-timeout=5s"
      "x-systemd.mount-timeout=5s"
      # Set samba credentials
      "credentials=/home/gaelan/smb-secrets"
      # Mount as user
      "uid=1000"
      "gid=100"
    ];
  };
}
