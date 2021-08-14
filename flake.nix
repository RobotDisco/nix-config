{
  description = "Gaelan's nix-based systems configuration";

  inputs = {
    # Flakes we're going to depend on
    nixpkgs.url = "github:nixos/nixpkgs/21.05";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    home-manager.url = "github:nix-community/home-manager/release-21.05";
    darwin.url = "github:lnl7/nix-darwin/master";

    # Hook up our chosen dependencies to be the ones our other dependencies use
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    darwin.inputs.nixpkgs.follows = "nixpkgs";
  };

  # This is where we define our own stuff
  outputs = { nixpkgs, darwin, emacs-overlay, home-manager, ... }@inputs:
    let
      common-nixos-modules = [
        ./nixos/profiles/common.nix
        {
          # I guess the nix overlays form doesn't do anything
          # magic, so I'm overriding it to add overlays
          # within the nixos module system itself?
          nixpkgs.overlays = with inputs; [
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
          nixpkgs.overlays = with inputs; [
            emacs-overlay.overlay
          ];
        }
      ];
    in

      {
          
        nixosConfigurations.arrakis = nixpkgs.lib.nixosSystem
          {
            system = "x86_64-linux";
            modules = common-nixos-modules ++ [
	            ./nixos/machines/arrakis
              # The module that loads home-manager
              home-manager.nixosModules.home-manager
              {
                # Maybe this will get shared, but for now use an anonymous
                # nix xpression to create my user
                users.users.gaelan = {
                  isNormalUser = true;
                  home = "/home/gaelan";
                  description = "Gaelan D'costa";
                  # TODO if I have role profiles elsewhere, these should be confingured there
                  extraGroups = [ "wheel" "networkmanager" "docker" "video" ];
                };
              }
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
