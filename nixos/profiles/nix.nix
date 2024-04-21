{
  imports = [
    ../../common/profiles/nix.nix
  ];

  config = {
    # Run garbage collection weekly
    nix.gc.dates = "weekly";
    # Optimise hardlinks daily for ad-hoc/derivation development reasons.
    nix.optimise.dates = "daily";
  };
}
