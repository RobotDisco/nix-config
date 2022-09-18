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
        darktower = lib.nixosSystem {
          system = "x86_64-linux";
          modules = [
            ./nixos/machines/darktower.nix
          ];
        };
        arrakis = myLib.nixosSystem {
          system = "x86_64-linux";
          configuration = {
            networking.hostName = "arrakis";
            robot-disco.hardware.framework.enable = true;

            services.autorandr = {
              enable = true;

              profiles = {
                default = {
                  fingerprint = {
                    eDP-1 = "00ffffffffffff0009e55f0900000000171d0104a51c137803de50a3544c99260f505400000001010101010101010101010101010101115cd01881e02d50302036001dbe1000001aa749d01881e02d50302036001dbe1000001a000000fe00424f452043510a202020202020000000fe004e4531333546424d2d4e34310a00fb";
                  };
                  config = {
                    eDP-1 = {
                      enable = true;
                      mode = "2256x1504";
                      position = "0x0";
                    };
                  };
                };
                clamshell = {
                  fingerprint = {
                    eDP-1 = "00ffffffffffff0009e55f0900000000171d0104a51c137803de50a3544c99260f505400000001010101010101010101010101010101115cd01881e02d50302036001dbe1000001aa749d01881e02d50302036001dbe1000001a000000fe00424f452043510a202020202020000000fe004e4531333546424d2d4e34310a00fb";
                    DP-1-1 = "00ffffffffffff0010ac7ba0535347300f1a0104a53420783aee95a3544c99260f5054a1080081408180a940b300d1c0010101010101283c80a070b023403020360006442100001a000000ff004854354e33363446304753530a000000fc0044454c4c2055323431324d0a20000000fd00323d1e5311000a202020202020009f";
                    DP-1-2 = "00ffffffffffff0010ac7ba04c31543035150104a53420783aee95a3544c99260f5054a1080081408180a940b300d1c0010101010101283c80a070b023403020360006442100001a000000ff004d324743523143533054314c0a000000fc0044454c4c2055323431324d0a20000000fd00323d1e5311000a2020202020200096";
                  };
                  config = {
                    eDP-1.enable = false;
                    DP-1-1 = {
                      enable = true;
                      primary = true;
                      mode = "1920x1200";
                      position = "0x0";
                    };
                    DP-1-2 = {
                      enable  = true;
                      mode = "1920x1200";
                      position = "1920x0";
                    };
                  };
                };
              };
            };
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
            }:$PATH sudo nixos-rebuild switch --flake . "$@"
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
