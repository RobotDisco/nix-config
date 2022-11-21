{ inputs, homeManagerSharedModules }:

let
  inherit (inputs) self home-manager nixpkgs;

  lib = nixpkgs.lib;

  pkgsForSystem = { system }:
    import nixpkgs {
      inherit system;

      overlays = [ inputs.emacs-overlay.overlays.default ] ++ lib.attrValues inputs.self.overlays;

      # Allow non-free/open source projects to be installed
      config.allowUnfree = true;
    };

in {
  # We use this function but I don't want to make this structure recursive. An
  # easy solution is to use =let= bindings and then also include the function in the output
  inherit pkgsForSystem;

  forAllSystems = let supportedSystems = [ "x86_64-linux" "x86_64-darwin" ];
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

      # There are some baseline settings we need to set up for this function
      # to work. Why does it work when lib.nixosSystem is directly called in
      # flake.nix? I do not know :)

      baseNixosModule = { nixpkgs = { inherit pkgs; }; };

      # my guess is this module is for my personal standard configuration for
      # home-manager in standard NixOS module style, vs what is in extraModules
      # which is the actual home-manager nixos module itself
      homeManagerModule = { config, ... }: {
        config.home-manager = {
          # Don't use a different nixpkgs derivation for each user; use the
          # global one I provide
          useGlobalPkgs = true;
          # install packages in /etc/profiles, not $HOME/.nix-profile
          useUserPackages = true;
          # This is where we provide our home-made home-manager modules
          sharedModules =
            homeManagerSharedModules; # ++ self.homeManagerModules;
        };
      };
    in lib.nixosSystem {
      # specialArgs is how we pass arguments into the module
      inherit system specialArgs;

      # My best guess is that modules are the modules _we_ generate, which
      # are effectively inline in a lot of configs
      modules = [ baseNixosModule homeManagerModule configuration ]
        ++ myModules;
      # My best guess is that these are external modules, i.e. those that
      # live in a standard place with a path
      extraModules = [
        # Always include these third-party modules as standard
        nixpkgs.nixosModules.notDetected
        home-manager.nixosModules.home-manager
      ] ++ contribModules;
    };

  homeManagerConfiguration = let
    homeDirectoryPrefix = pkgs:
      if pkgs.stdenv.hostPlatform.isDarwin then "/Users" else "/home";
  in { username, configuration, system
  , pkgs ? (pkgsForSystem { inherit system; })
  , homeDirectory ? "${homeDirectoryPrefix pkgs}/${username}" }:
  home-manager.lib.homeManagerConfiguration {
    inherit configuration username homeDirectory system pkgs;

    extraModules = homeManagerSharedModules;
  };
}
