{ lib, inputs, ... }:

let
  pkgsForSystem = { system }:
    let
      home-manager = inputs.home-manager;

      homeManagerOverlay = final: prev: {
        home-manager = home-manager.defaultPackage.${system};
      };

      inputOverlays = [ homeManagerOverlay ];
    in import inputs.nixpkgs {
      inherit system;

      overlays = inputOverlays;
      # Allow non-free/open source projects to be installed
      config.allowUnfree = true;
    };

in {
  # We use this function but I don't want to make this structure recursive. An
  # easy solution is to use =let= bindings and then also include the function in the output
  inherit pkgsForSystem;

  forAllSystems = let supportedSystems = [ "x86_64-linux" ];
  in func:
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
  (system: func (pkgsForSystem { inherit system; }));

  nixosSystem =
    # The CPU architecture of the host being generated
    { system ? "x86_64-linux"
      # NixOS configuration
    , configuration ? { }
      # modules I wrote that should be included
    , myModules ? [ ]
      # third-party modules not written by me
    , contribModules ? [ ]
      # Any extra overlays that should be included
    , extraOverlays ? [ ]
      # Pass a set of arguments in here you want accessible to all modules
    , specialArgs ? { }, ... }:
    let
      pkgs = pkgsForSystem { inherit system; };

      baseNixosModule = {
        # This makes it easier to know what version of a module I am running?
        system.configurationRevision =
          lib.mkIf (inputs.self ? rev) inputs.self.rev;
        # This is a nix store, which is an important concept in nix. 
        nixpkgs = { inherit pkgs; };
        # Add my generated nix store to the search path
        nix.nixPath = [ "nixpkgs=${inputs.nixpkgs}" ];
        # Since my nix store is a flake, add it to the registry
        nix.registry.nixpkgs.flake = inputs.nixpkgs;
      };

      homeManagerNixosModule = { config, ... }: {
        config.home-manager = {
          useGlobalPkgs = true;
          useUserPackages = true;

          # This is how we pass arguments into the module.
          extraSpecialArgs = specialArgs;
          # TODO Terlar had this in his base nix flake file. I've split mine
          # out so how do I get these in?
          sharedModules = [ ];
        };
      };
    in lib.nixosSystem {
      # specialArgs is how we pass arguments into the module
      inherit system specialArgs;

      # My best guess is that modules are the modules _we_ generate, which
      # are effectively inline in a lot of configs
      myModules = [ baseNixosModule homeManagerNixosModule configuration ]
        ++ myModules;
      # My best guess is that these are external modules, i.e. those that
      # live in a standard place with a path
      contribModules = [
        inputs.nixpkgs.nixosModules.notDetected
        inputs.home-manager.nixosModules.home-manager
      ] ++ (lib.attrValues inputs.self.nixosModules) ++ contribModules;
    };
}
