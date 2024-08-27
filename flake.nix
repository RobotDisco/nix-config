{
  description = "Gaelan's nix-based systems configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.05";
    nixpkgs-mac.url = "github:nixos/nixpkgs/nixpkgs-24.05-darwin";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixpkgs-unstable";

    # Secrets management
    # Using a fork of flake-nix as I wait for
    # https://github.com/ryantm/agenix/pull/273
    # to get in.
    agenix = {
      url = "github:RobotDisco/agenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # My private secrets repository.
    # use ssh protocol to authenticate via ssh-agent/ssh-key
    # and shallow clone to save time.
    # Idea from https://github.com/ryan4yin/nix-config
    robotdisco-secrets = {
      url = "git+ssh://git@github.com/RobotDisco/nix-secrets.git?shallow=1";
      flake = false;
    };

    darwin.url = "github:lnl7/nix-darwin/master";
    darwin.inputs.nixpkgs.follows = "nixpkgs-mac";

    emacs-overlay.url = "github:nix-community/emacs-overlay";
    emacs-overlay.inputs.nixpkgs.follows = "nixpkgs";

    nixos-hardware.url = "github:NixOS/nixos-hardware/master";

    home-manager.url = "github:nix-community/home-manager/release-24.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    robonona.url = "github:RobotDisco/robonona-clj";
    robonona.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = inputs@{ self, nixpkgs, darwin, emacs-overlay
    , home-manager, nixos-hardware, ... }:
    let
      inherit (nixpkgs) lib;

      ### HERE BEGINS WHAT IS EFFECTIVELY MY CONFIGURATION SECTION
      # What platforms do I support?
      supportedSystems = [ "x86_64-linux" "aarch64-darwin" ];
      #### THIS ENDS WHAT IS EFFECTIVELY MY CONFIGURATION SECTION.

      ### HERE BEGINS MY HELPER FUNCTION LIBRARY ###
      # for a list of system strings, and a function which takes a string argument,
      # for each system string list, produce an attribute set where the key is
      # the string and the value is the result of applying that function to the string.
      forEachSystem = systems: func: lib.genAttrs systems func;
      # Like the above but with our supportedSystems list partially applied.
      # remember that nix functions can be curried, so we can pre-supply the first
      # argument to return a function that only needs the second.
      forAllSystems = forEachSystem supportedSystems;
      # Darwin config generator
      darwinSystem = import ./lib/darwinSystem.nix;
      # NixOS config generator
      nixosSystem = import ./lib/nixosSystem.nix;
      ### HERE ENDS MY HELPER FUNCTION LIBRARY ###
    in {
      darwinConfigurations = {
        "Fountain-of-Ahmed-III" = darwinSystem {
          inherit darwin home-manager;
          system = "aarch64-darwin";
          darwinModules = [
            ./machines/Fountain-of-Ahmed-III.nix
          ];
          darwinSpecialArgs = {};
          homeModules = [
            inputs.agenix.homeManagerModules.default
            ./secrets/home-manager.nix
          ];
          homeSpecialArgs = { inherit (inputs) agenix robotdisco-secrets; };
        };
      };

      nixosConfigurations = {
        arrakis = nixosSystem {
          inherit nixpkgs home-manager;
          system = "x86_64-linux";
          nixosModules = [
            inputs.nixos-hardware.nixosModules.framework-13-7040-amd
            ./machines/arrakis
          ];
          nixosSpecialArgs = { inherit (inputs) agenix robotdisco-secrets; };
          homeModules = [
            inputs.agenix.homeManagerModules.default
            ./secrets/home-manager.nix
          ];
          homeSpecialArgs = { inherit (inputs) agenix robotdisco-secrets; };
        };
        darktower = nixosSystem {
          inherit nixpkgs home-manager;
          system = "x86_64-linux";
          homeModules = [];
          homeSpecialArgs = {};
          nixosModules = [
            ./machines/darktower.nix
              # Secure secret injection
            inputs.agenix.nixosModules.default
            ./secrets/nixos.nix
            {
              systemd.timers."robonona" = {
                enable = true;
                wantedBy = [ "timers.target" ];
                timerConfig = {
                  OnCalendar = "Mon *-*-* 08:30";
                  RandomizedDelaySec = "300";
                  Unit = "robonona.service";
                };
              };

              systemd.services."robonona" = {
                enable = true;
                wants = [ "network-online.target" ];
                script = ''
                  set -eu
                  ${inputs.robonona.packages.x86_64-linux.default}/bin/robonona prod
                '';
                serviceConfig = {
                  Type = "oneshot";
                  User = "gaelan";
                };
              };
            }
          ];
          nixosSpecialArgs = { inherit (inputs) robotdisco-secrets; };
        };
      };

      apps = forAllSystems (system:
        let pkgs = nixpkgs.legacyPackages."${system}";
        in
        pkgs.lib.trivial.pipe [
          # This list is honestly all I want to see here
          # possibly, even hiding the fact that it is an
          # application of writeShell Application.
          #
          # Everything else is transformation stuff that is
          # used to minimize the amount of boilerplate written
          # and should be encapsulated somewhere else, like in
          # lib/
          {
            name = "switch";
            text = if pkgs.stdenv.isDarwin
              then "darwin-rebuild switch --flake ${toString ./.}#"
              else "sudo nixos-rebuild switch --flake ${toString ./.}#";
          }
          {
            name = "use-caches";
            runtimeInputs = [ pkgs.cachix ];
            text = ''
              ${pkgs.cachix}/bin/cachix use -O . nix-community
              ${pkgs.cachix}/bin/cachix use -O . robot-disco
            '';
          }
        ] [
          # Generate a derivation
          (builtins.map pkgs.writeShellApplication)
          # Transform derivation into flakes app item schema
          (builtins.map (deriv: {
            name = deriv.name;
            value = {
              type = "app";
              program = "${deriv}/bin/${deriv.name}";
            };
          }))
          # Convert list of app objects into attrset
          builtins.listToAttrs
        ]);

      devShells = forAllSystems (system:
        let pkgs = nixpkgs.legacyPackages."${system}";
        in
          {
            default = pkgs.mkShell {
              nativeBuildInputs = with pkgs; [
                # Linter
                statix
                # Check for unused imports and other dead code
                deadnix
                # Upcoming official nixpkgs formatter
                nixfmt-rfc-style ];
            };
          });

      # Run ~nix fmt~ to use this package to format nix files
      formatter = forAllSystems (system:
        nixpkgs.legacyPackages."${system}".nixfmt-rfc-style);

      # Conceptually it feels like I should be defining my packages
      # in the packages settings and then defining overlays that reference
      # my flake packages. However, since I'm using the emacs overlay to
      # derive my configs and need my packages in almost every flake item to have
      # my emacs packages introduced by overlay, it was easier to define it the
      # other way around.
      overlays = {
        emacs = final: prev: import ./overlays/emacs final prev;
      };
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
