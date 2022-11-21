{self, home-manager, nixpkgs, ...}@inputs:

let
  supportedSystems = [ "x86_64-linux" "x86_64-darwin" ];

  lib = nixpkgs.lib;

  baselineOverlays = [
    inputs.emacs-overlay.overlays.default
  ] ++ lib.attrValues self.overlays;

  baselineHomeManagerModules = lib.attrValues self.homeManagerModules;

  pkgsForSystem = system:
  import nixpkgs {
    inherit system;

    overlays = baselineOverlays;

    # Allow non-free/open source projects to be installed
    config.allowUnfree = true;
  };

  forAllSystems = func:
  lib.genAttrs supportedSystems
  (system: func (pkgsForSystem system));

  nixosSystem =
    # The CPU architecture of the host being generated
    { system
    # NixOS modules or inline configuration
    , modules ? []
    , homeManagerModules ? []
    # paramaters to inject into every module 
    , specialArgs ? { }
    }:
    let
      baseNixosModules = [
        nixpkgs.nixosModules.notDetected
        {
          #nixpkgs.pkgs = pkgsForSystem { inherit system; };
          nixpkgs.overlays = baselineOverlays;
        }
      ];

      homeManagerModules = [
        home-manager.nixosModules.home-manager
        {
          home-manager = {
            useGlobalPkgs = true;
            useUserPackages = true;

            sharedModules =
              baselineHomeManagerModules;
          };
        }
      ];

      #myNixosModules = (lib.attrValues self.nixosModules);
      myNixosModules = [];
    in
    lib.nixosSystem {
      inherit system specialArgs;

      modules = 
      baseNixosModules ++ homeManagerModules ++ myNixosModules ++ modules;
    };

    homeManagerConfiguration = let
      homeDirectoryPrefix = pkgs:
      if pkgs.stdenv.hostPlatform.isDarwin then "/Users" else "/home";
    in { username, configuration, system
    , pkgs ? (pkgsForSystem { inherit system; })
    , homeDirectory ? "${homeDirectoryPrefix pkgs}/${username}" }:
    home-manager.lib.homeManagerConfiguration {
      inherit configuration username homeDirectory system pkgs;

      extraModules = baselineHomeManagerModules;
    };

in { inherit forAllSystems nixosSystem homeManagerConfiguration; }
