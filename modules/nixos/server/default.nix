{
  imports = [
    ../common

    ../hardware/ssd.nix

    ./mail.nix
    ./ups.nix
    ./zfs.nix
  ];

  config = {
    # Enable remote access via SSH
    services.openssh.enable = true;
  };
}
