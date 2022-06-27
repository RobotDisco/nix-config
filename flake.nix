{
  description = "Gaelan's nix-based systems configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-22.05";
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";

    gaelan-emacs.url = "github:RobotDisco/emacs-config";
    gaelan-emacs.inputs.nixpkgs.follows = "nixpkgs";

    home-manager.url = "github:nix-community/home-manager/release-22.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = inputs@{ self, nixpkgs, gaelan-emacs, home-manager, nixos-hardware }:
    let
      inherit (nixpkgs) lib;

      myLib = import ./lib {
        inherit inputs;

        inherit homeManagerSharedModules;
      };

      # Are there home-manager modules we will want to include?
      # home-manager calls these "sharedModules" for whatever reason.
      homeManagerSharedModules = [
        gaelan-emacs.homeManagerModules.emacsConfig
      ]
      # Here's an interesting thing we do, taking advantage of Nix/Haskell's
      # laziness. We should include the modules we define ourselves. The flake
      # schema has us export those as "homeManagerModules". Even though the
      # field is likely defined after this line (we don't want to expose this
      # variable to the outside world so it's defined early in a let binding)
      # we can still refer to a field that is defined later.
      # Given recursive records (records that have fields which refer to other
      # fields defined in the record) are an anti-pattern, this is a common
      # tactic. The only danger is when self.<field> winds up getting changed
      # by some other imported module in a way we don't like.
        ++ self.homeManagerModules;
    in {
      homeConfigurations = {
        gaelan = myLib.homeManagerConfiguration {
          username = "gaelan.dcosta";
          system = "x86_64-darwin";
          configuration = {
            robot-disco.emacs.enable = true;
          };
        };
      };
      
      homeManagerModules = [
        ./home-manager/modules/user/gaelan
      ];

      nixosModules = {
        default = (import ./nixos/modules {});
      };

      nixosConfigurations = {
        arrakis = myLib.nixosSystem {
          system = "x86_64-linux";
          configuration = {
            networking.hostName = "arrakis";
            robot-disco.hardware.framework.enable = true;
	  };
	  myModules = lib.attrValues self.nixosModules;
          contribModules = [ nixos-hardware.nixosModules.framework ];
        };
      };

      apps = myLib.forAllSystems (pkgs:
        lib.mapAttrs
        # First function arg is key, second is value 
        (binary: derivation: {
          type = "app";
          program = "${derivation}/bin/${binary}";
        }) {
          home-switch = pkgs.writers.writeBashBin "home-switch" ''
            ${pkgs.home-manager}/bin/home-manager switch --flake .#"$@"
          '';

          use-caches = pkgs.writers.writeBashBin "use-caches" ''
            ${pkgs.cachix}/bin/cachix use -O . nix-community
          '';

          nixos-switch = pkgs.writers.writeBashBin "nixos-switch" ''
            PATH=${
              lib.makeBinPath [ pkgs.gitMinimal pkgs.nix pkgs.nixos-rebuild ]
            }:$PATH nixos-rebuild switch --flake . "$@"
          '';
        });

      devShells = myLib.forAllSystems (pkgs:
        {
	  default = pkgs.mkShell {
            nativeBuildInputs = [ pkgs.git pkgs.nix pkgs.nixfmt ];

            shellHook = "  export NIX_USER_CONF_FILES=${toString ./.}/nix.conf\n";
         };
      });
    };
}
