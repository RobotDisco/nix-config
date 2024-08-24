{ pkgs, ... }:

{
  # Annoyingly, MacOS X's launchd has a totally different structure for setting
  # up timed tasks than Linux's systemd. So we need to specify timing
  # differently.

  imports = [ ../common/nix.nix ];

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

    # Sometimes it's easier / more feasible to use legacy nix commands that rely
    # on the old <nixpkgs> channel. Since nix flakes don't use traditional
    # nix channels, let's set it here to pin <nixpkgs> to our current nixpkgs
    # input.
    #
    # On NixOS this is done for us. On MacOS X we have to do it ourselves.
    environment.variables.NIX_PATH = [ "nixpkgs=${pkgs.path}" ];
  };

}
