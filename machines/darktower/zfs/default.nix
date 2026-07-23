{
  imports = [
    ./local-snapshots.nix
    ./remote-backup.nix
  ];

  config = {
    # This should be parameterized, an eight-character hex string
    networking.hostId = "aa3d3177";

    # I don't care about specific mountpoints, so just mount the pools
    boot.zfs = {
      # Proactively set this to false, which will be the default in 26.11
      forceImportRoot = false;

      extraPools = [
        "storagepool"
        "backuppool"
      ];
    };
  };
}
