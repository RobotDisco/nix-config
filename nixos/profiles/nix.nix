{
  # Different operating systems have different ways of configuring timed tasks;
  # in this case, nix and darwin don't have a unified configuration structure,
  # but lean on the native structures.

  imports = [ ../../modules/common/nix.nix ];

  config = {
    # Run garbage collection weekly
    nix.gc.dates = "weekly";
    # Optimise hardlinks daily for ad-hoc/derivation development reasons.
    nix.optimise.dates = [ "daily" ];
  };
}
