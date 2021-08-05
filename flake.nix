{
  description = "Gaelan's nix-based systems configuration";

  inputs = {
    # Flakes we're going to depend on
    nixpkgs.url = "github:nixos/nixpkgs/21.05";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    home-manager.url = "github:nix-community/home-manager/release-21.05";

    # Hook up our chosen dependencies to be the ones our other dependencies use
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };

  # This is where we define our own stuff
  outputs = { self, nixpkgs, emacs-overlay, home-manager, ... }@inputs:
    let
      timeZone = "America/Toronto"; # time.timeZone
    in
      
    {
      overlays = {
        emacs-overlay = emacs-overlay.overlay;
      };

      nixosConfigurations.arrakis = nixpkgs.lib.nixosSystem
        {
          system = "x86_64-linux";

          # This seems like a hack to inject overlays outside
          # of where they are supposed to be injected (overlays above)
          specialArgs = { inherit inputs; };
          
          modules = [
	    ./machines/arrakis
            home-manager.nixosModules.home-manager
	    {
	        home-manager.useUserPackages = true;
		home-manager.useGlobalPkgs = true;
		home-manager.users.gaelan =
      		  import ./users/gaelan/home-manager.nix;
            }
          ];
        };
    };
}
