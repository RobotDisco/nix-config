{ lib, nixpkgs }:

rec {
  pkgsForSystem = { system }:
    import nixpkgs {
      inherit system;
    };

  forAllSystems =
    let
      supportedSystems = [ "x86_64-linux" ];
    in
      func:
        # genAttrs takes each item in supportedSystems, uses them as names
	# in a set, and passes it as an argument to a function (named func)
	# to produce their respective values.
	lib.genAttrs supportedSystems
	  # We need the per-system concrete pkgs item for almost every use
	  # of the func that will be passed in, but I don't want to write
	  # boilerplate for that every time. Let's leverage closures for
	  # this. (i.e. we expect uses of this function to expect a pkgs
	  # argument but we will always supply it.
	  #
	  # Why not use a let? Because requiring your supplied functions to
	      # provide a slot for packages makes the expectation explicit?
	  (system: func (pkgsForSystem {
	    inherit system;
          }));
}