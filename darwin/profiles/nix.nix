{
  imports = [
    ../../common/profiles/nix.nix
  ];

  config = {
    # Run garbage collection weekly, around dev-all-hands
    nix.gc.interval = {
      Weekday = 3;
      Hour = 14;
      Minute = 30;
    };
    # Optimise hardlinks daily for ad-hoc/derivation development reasons.
    # Run just after scrum
    nix.optimise.interval = {
      Hour = 11;
      Minute = 45;
    };
  };
}
