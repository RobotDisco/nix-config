{
  description = "Gaelan's nix-based systems configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.05";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixpkgs-unstable";

    # Secrets management
    agenix = {
      url = "github:ryantm/agenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    agenix-rekey = {
      url = "github:oddlama/agenix-rekey";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    darwin.url = "github:lnl7/nix-darwin/nix-darwin-25.05";

    emacs-overlay.url = "github:nix-community/emacs-overlay";
    emacs-overlay.inputs.nixpkgs.follows = "nixpkgs";

    home-manager.url = "github:nix-community/home-manager/release-25.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    nixos-hardware.url = "github:NixOS/nixos-hardware/master";

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
      self,
      nixpkgs,
      darwin,
      emacs-overlay,
      home-manager,
      nixos-hardware,
      ...
    }:
    let
      inherit (nixpkgs) lib;

      # My custom functions with all required inputs
      myLib = import ./lib {
        inherit
          lib
          nixpkgs
          emacs-overlay
          home-manager
          ;
        inherit (inputs) darwin nixpkgs-unstable;
      };
      inherit (myLib) forAllSystems darwinSystem nixosSystem;
    in
    ### HERE ENDS MY HELPER FUNCTION LIBRARY ###
    {
      checks = import ./checks.nix { inherit nixpkgs forAllSystems; };
      darwinConfigurations = {
        fountain-of-ahmed-iii = darwinSystem {
          system = "aarch64-darwin";
          modules = [
            ./machines/fountain-of-ahmed-iii.nix
            inputs.agenix.darwinModules.default
            inputs.agenix-rekey.nixosModules.default
          ];
          specialArgs = {
            inherit myLib;
            inherit (inputs) robotdisco-secrets;
          };
          homeModules = [
            inputs.agenix.homeManagerModules.default
            inputs.agenix-rekey.homeManagerModules.default
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
            inputs.agenix-rekey.nixosModules.default
            (
              {
                pkgs,
                ...
              }:
              # See https://github.com/systemd/systemd/issues/34304
              {
                systemd.package = pkgs.systemd.overrideAttrs (old: {
                  patches = old.patches ++ [
                    (pkgs.fetchurl {
                      url = "https://github.com/wrvsrx/systemd/compare/tag_fix-hibernate-resume%5E...tag_fix-hibernate-resume.patch";
                      hash = "sha256-Z784xysVUOYXCoTYJDRb3ppGiR8CgwY5CNV8jJSLOXU=";
                    })
                  ];
                });
              }
            )
          ];
          specialArgs = {
            inherit myLib;
            inherit (inputs) agenix robotdisco-secrets;
          };
          homeModules = [
            inputs.agenix.homeManagerModules.default
            inputs.agenix-rekey.homeManagerModules.default
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
            # Secure secret injection
            inputs.agenix.nixosModules.default
            inputs.agenix-rekey.nixosModules.default
          ];
          specialArgs = {
            inherit myLib;
            inherit (inputs) agenix robotdisco-secrets;
          };
          homeModules = [ ];
          homeSpecialArgs = { inherit inputs; };
        };
      };

      # Expose the necessary information in your flake as an output so
      # agenix-rekey knows where it has to look for secrets and paths.
      #
      # Make sure that the pkgs passed here comes from the same nixpkgs version
      # as the pkgs used on your hosts in `nixosConfigurations`, otherwise the
      # rekeyed derivations will not be found!
      agenix-rekey = inputs.agenix-rekey.configure {
        userFlake = self;
        nixosConfigurations = self.nixosConfigurations // self.darwinConfigurations;
      };

      devShells = import ./devshells.nix { inherit nixpkgs inputs forAllSystems; };

      # Run ~nix fmt~ to use this package to format nix files
      formatter = forAllSystems (system: nixpkgs.legacyPackages."${system}".nixfmt-rfc-style);

      packages."x86_64-linux" =
        let
          pkgs = import nixpkgs {
            system = "x86_64-linux";
            config.allowUnfree = true;
          };
        in
        import ./packages/default.nix { inherit pkgs; };
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
