{
  description = "Gaelan's nix-based systems configuration";

  inputs = {
    # Flakes we're going to depend on
    nixpkgs.url = github:nixos/nixpkgs/21.05;
    emacs-overlay.url = github:nix-community/emacs-overlay;
    home-manager.url = github:nix-community/home-manager/release-21.05;
    darwin.url = github:lnl7/nix-darwin/master;
    deploy-rs.url = github:serokell/deploy-rs;

    # Hook up our chosen dependencies to be the ones our other dependencies use
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    darwin.inputs.nixpkgs.follows = "nixpkgs";
  };

  # This is where we define our own stuff
  outputs = { self, nixpkgs, darwin, deploy-rs, emacs-overlay, home-manager, ... }@inputs:
    let
      common-nixos-modules = [
        ./nixos/profiles/common.nix
        ({ users.mutableUsers = false; })
        ./nixos/users/gaelan.nix
        {
          # I guess the nix overlays form doesn't do anything
          # magic, so I'm overriding it to add overlays
          # within the nixos module system itself?
          nixpkgs.overlays = [
            emacs-overlay.overlay
          ];
        }
      ];

      common-darwin-modules = [
        ./darwin/profiles/common.nix
        {
          # I guess the nix overlays form doesn't do anything
          # magic, so I'm overriding it to add overlays
          # within the nixos module system itself?
          nixpkgs.overlays = [
            emacs-overlay.overlay
          ];
        }
      ];
    in

      {
          
        nixosConfigurations = {
          arrakis = nixpkgs.lib.nixosSystem
            {
              system = "x86_64-linux";
              modules = common-nixos-modules ++ [
                ./nixos/profiles/baremetal.nix
                ./nixos/machines/arrakis
                # The module that loads home-manager
                home-manager.nixosModules.home-manager
                # My anonymous module that has some (probably oughta be common
                # settings and my user's customized home-manager config
	              {
	                home-manager.useUserPackages = true;
		              home-manager.useGlobalPkgs = true;
		              home-manager.users.gaelan =
      		          import ./home-manager/users/gaelan/default.nix;
                }
              ];
            };
          darktower = nixpkgs.lib.nixosSystem
            {
              system = "x86_64-linux";

              modules = common-nixos-modules ++ [
                ./nixos/profiles/baremetal.nix
                ./nixos/machines/darktower
                ./nixos/profiles/hypervisor.nix
                ./nixos/profiles/hardware/ups.nix
              ];
            };
          chapterhouse = nixpkgs.lib.nixosSystem {
            system = "x86_64-linux";

            modules = common-nixos-modules ++ [
              ./nixos/profiles/kvm-guest.nix
              { networking.hostName = "chapterhouse"; }
            ];
          };
        };

        deploy.nodes.darktower = {
          fastConnection = true;
          user = "root";
          sshUser = "gaelan";
          hostname = "192.168.10.3";
          profiles.system = {
            path = deploy-rs.lib.x86_64-linux.activate.nixos self.nixosConfigurations.darktower;
          };
        };

        checks = builtins.mapAttrs (system: deployLib: deployLib.deployChecks self.deploy) deploy-rs.lib;

        darwinConfigurations.caladan = darwin.lib.darwinSystem {
          modules = common-darwin-modules ++ [
            home-manager.darwinModules.home-manager
            {
              # Set laptop hostname
              networking.hostName = "caladan";
            }
            {
              # Set up my name and make sure it runs zsh
              users.users."gaelan.dcosta" = {
                description = "Gaelan D'costa";
                shell = nixpkgs.zsh;
              };
            }
            {
              # My anonymous module that has some (probably oughta be common
              # settings and my user's customized home-manager config
              home-manager.useUserPackages = true;
		          home-manager.useGlobalPkgs = true;
		          home-manager.users."gaelan.dcosta" =
      		      import ./home-manager/users/gaelan/default.nix;
            }
          ];
        };
      };
}
