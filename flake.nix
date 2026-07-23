{
  description = "Gaelan's nix-based systems configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-26.05";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixpkgs-unstable";

    # Secrets management
    agenix = {
      url = "github:yaxitech/ragenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    darwin = {
      url = "github:lnl7/nix-darwin/nix-darwin-26.05";
    };

    emacs-overlay.url = "github:nix-community/emacs-overlay";
    emacs-overlay.inputs.nixpkgs.follows = "nixpkgs";

    home-manager.url = "github:nix-community/home-manager/release-26.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    mac-app-utils = {
      url = "github:hraban/mac-app-util";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixos-hardware = {
      url = "github:NixOS/nixos-hardware/master";
    };

    # My private secrets repository.
    # use ssh protocol to authenticate via ssh-agent/ssh-key
    # and shallow clone to save time.
    # Idea from https://github.com/ryan4yin/nix-config
    robotdisco-secrets = {
      url = "git+ssh://git@github.com/RobotDisco/nix-secrets.git?shallow=1";
      #url = "path:/Users/gaelan/code/nix-secrets";
      flake = false;
    };
  };

  outputs =
    inputs@{
      nixpkgs,
      darwin,
      emacs-overlay,
      home-manager,
      ...
    }:
    let
      # My custom functions with all required inputs
      myLib = import ./lib {
        inherit (nixpkgs) lib;
        inherit (inputs)
          darwin
          nixpkgs
          nixpkgs-unstable
          emacs-overlay
          home-manager
          ;
      };
      inherit (myLib) darwinSystem nixosSystem;
    in
    ### HERE ENDS MY HELPER FUNCTION LIBRARY ###
    {
      checks = import ./checks.nix {
        nixpkgs = inputs.nixpkgs-unstable;
      };
      darwinConfigurations = {
        fountain-of-ahmed-iii = darwinSystem {
          system = "aarch64-darwin";
          modules = [
            ./machines/fountain-of-ahmed-iii.nix
            inputs.agenix.darwinModules.default
            inputs.mac-app-utils.darwinModules.default
          ];
          specialArgs = {
            inherit myLib;
            inherit (inputs) robotdisco-secrets;
          };
          homeModules = [
            inputs.agenix.homeManagerModules.default
            inputs.mac-app-utils.homeManagerModules.default
          ];
          homeSpecialArgs = {
            inherit myLib;
            inherit (inputs) agenix robotdisco-secrets;
          };
        };
      };

      nixosConfigurations = {
        arrakis = nixosSystem {
          system = "x86_64-linux";
          modules = [
            inputs.nixos-hardware.nixosModules.framework-13-7040-amd
            ./machines/arrakis
            inputs.agenix.nixosModules.default
          ];
          specialArgs = {
            inherit myLib;
            inherit (inputs) agenix robotdisco-secrets;
          };
          homeModules = [
            inputs.agenix.homeManagerModules.default
          ];
          homeSpecialArgs = {
            inherit myLib;
            inherit (inputs) agenix robotdisco-secrets;
          };
        };
        darktower = nixosSystem {
          system = "x86_64-linux";
          modules = [
            ./machines/darktower
            inputs.agenix.nixosModules.default
          ];
          specialArgs = {
            inherit myLib;
            inherit (inputs) agenix robotdisco-secrets;
          };
          homeModules = [ ];
        };
      };

      homeConfigurations = {
        "gaelan@arrakis" =
          let
            pkgs = import nixpkgs {
              system = "x86_64-linux";
              config.allowUnfree = true;
              overlays = [
                emacs-overlay.overlays.default
                (import ./packages/overlay.nix)
              ];
            };
            pkgs-unstable = import inputs.nixpkgs-unstable {
              system = "x86_64-linux";
              config.allowUnfree = true;
            };
          in
          inputs.home-manager.lib.homeManagerConfiguration {
            inherit pkgs;
            modules = [
              ./home-manager/modules
              ./home-manager/profiles/gaelan-personal.nix
              inputs.agenix.homeManagerModules.default
            ];
            extraSpecialArgs = {
              inherit myLib pkgs-unstable;
              inherit (inputs) robotdisco-secrets;
              hostName = "arrakis";
            };
          };
        "gaelan@fountain-of-ahmed-iii" =
          let
            pkgs = import nixpkgs {
              system = "aarch64-darwin";
              config.allowUnfree = true;
              overlays = [
                emacs-overlay.overlays.default
                (import ./packages/overlay.nix)
              ];
            };
            pkgs-unstable = import inputs.nixpkgs-unstable {
              system = "aarch64-darwin";
              config.allowUnfree = true;
            };
          in
          inputs.home-manager.lib.homeManagerConfiguration {
            inherit pkgs;
            modules = [
              ./home-manager/modules
              ./home-manager/profiles/gaelan-work.nix
              inputs.agenix.homeManagerModules.default
              inputs.mac-app-utils.homeManagerModules.default
            ];
            extraSpecialArgs = {
              inherit myLib pkgs-unstable;
              inherit (inputs) robotdisco-secrets;
              hostName = "fountain-of-ahmed-iii";
            };
          };
      };

      devShells = import ./devshells.nix { inherit nixpkgs; };

      # Run ~nix fmt~ to use this package to format nix files
      formatter = {
        x86_64-linux = nixpkgs.legacyPackages.x86_64-linux.nixfmt;
        aarch64-darwin = nixpkgs.legacyPackages.aarch64-darwin.nixfmt;
      };

      packages = nixpkgs.lib.genAttrs [ "x86_64-linux" "aarch64-darwin" "aarch64-linux" ] (
        system:
        let
          pkgs = import nixpkgs {
            inherit system;
            config.allowUnfree = true;
            overlays = [ emacs-overlay.overlays.default ];
          };
        in
        import ./packages/default.nix { inherit pkgs; }
      );
    };

  # Supply a project-specific attribute set of nix configuration
  # for this project specifically. Enable flakes, and authorize the use of some
  # additional derivation caches.
  nixConfig = {
    experimental-features = [
      "nix-command"
      "flakes"
    ];
    extra-substituters = [
      "https://nix-community.cachix.org"
      "https://robot-disco.cachix.org"
    ];
    extra-trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "robot-disco.cachix.org-1:UOaR4+SF1stx8O/Z+bJD9ENNjumfabRNRvCnjwct0sg="
    ];
  };
}
