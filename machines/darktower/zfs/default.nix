{
  imports = [
    ./local-snapshots.nix
    ./remote-backup.nix
  ];

  config = {
    # This should be parameterized, an eight-character hex string
    networking.hostId = "aa3d3177";

    # I don't care about specific mountpoints, so just mount the pools
    boot.zfs.extraPools = [
      "storagepool"
      "backuppool"
    ];
  };
}
