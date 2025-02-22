{
  description = "Gaelan's nix-based systems configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.11";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixpkgs-unstable";

    # Secrets management
    # Using a fork of flake-nix as I wait for
    # https://github.com/ryantm/agenix/pull/273
    # to get in.
    agenix = {
      url = "github:RobotDisco/agenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    darwin.url = "github:lnl7/nix-darwin/nix-darwin-24.11";

    emacs-overlay.url = "github:nix-community/emacs-overlay";
    emacs-overlay.inputs.nixpkgs.follows = "nixpkgs";

    home-manager.url = "github:nix-community/home-manager/release-24.11";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    nixos-hardware.url = "github:NixOS/nixos-hardware/master";

    pre-commit-hooks.url = "github:cachix/git-hooks.nix";

    # My private secrets repository.
    # use ssh protocol to authenticate via ssh-agent/ssh-key
    # and shallow clone to save time.
    # Idea from https://github.com/ryan4yin/nix-config
    robotdisco-secrets = {
      url = "git+ssh://git@github.com/RobotDisco/nix-secrets.git?shallow=1";
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

      ### HERE BEGINS WHAT IS EFFECTIVELY MY CONFIGURATION SECTION
      # What platforms do I support?
      supportedSystems = [
        "x86_64-linux"
        "aarch64-darwin"
      ];
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
      darwinSystem = import ./lib/darwinSystem.nix [
        emacs-overlay.overlays.default
        self.overlays.emacs
      ];
      # NixOS config generator
      nixosSystem = import ./lib/nixosSystem.nix (
        [
          (_final: prev: {
            emacsPackages = prev.emacsPackages // {
              inherit (inputs.nixpkgs-unstable.legacyPackages."${prev.system}".emacsPackages) dap-mode;
            };
          })
          emacs-overlay.overlays.default
        ]
        ++ (lib.attrValues self.overlays)
      );
      # My custom functions
      myLib = import ./lib { inherit lib; };
    in
    ### HERE ENDS MY HELPER FUNCTION LIBRARY ###
    {
      checks = forAllSystems (system: {
        pre-commit-check = inputs.pre-commit-hooks.lib.${system}.run {
          excludes = [ "hardware-configuration\\.nix" ];
          hooks = {
            # Github actions linter
            actionlint.enable = true;
            # Detect unused function inputs
            deadnix = {
              enable = true;
            };
            # Detect unsupported NixOS input versions
            flake-checker = {
              # Enable when they finally upgrade flake-checker to work with 24.11
              #enable = true;
            };
            # Do something via Nix language server?
            nil = {
              enable = true;
            };
            # RFC-compliant nix format checker
            nixfmt-rfc-style = {
              enable = true;
            };
            # nix static analysis
            statix = {
              enable = true;
              # For some stupid reason statix doesn't use the standard
              # excludes flag but uses a separate "ignores" field.
              settings.ignore = [ "hardware-configuration.nix" ];
            };
          };
          src = ./.;
        };
      });
      darwinConfigurations = {
        "Fountain-of-Ahmed-III" = darwinSystem {
          inherit darwin home-manager;
          system = "aarch64-darwin";
          darwinModules = [ ./machines/Fountain-of-Ahmed-III.nix ];
          darwinSpecialArgs = {
            inherit myLib;
          };
          homeModules = [
            inputs.agenix.homeManagerModules.default
            ./secrets/home-manager.nix
          ];
          homeSpecialArgs = {
            inherit myLib;
            inherit (inputs) agenix robotdisco-secrets;
          };
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
          nixosSpecialArgs = {
            inherit myLib;
            inherit (inputs) agenix robotdisco-secrets;
          };
          homeModules = [
            inputs.agenix.homeManagerModules.default
            ./secrets/home-manager.nix
          ];
          homeSpecialArgs = {
            inherit myLib;
            inherit (inputs) agenix robotdisco-secrets;
          };
        };
        darktower = nixosSystem {
          inherit nixpkgs home-manager;
          system = "x86_64-linux";
          homeModules = [ ];
          homeSpecialArgs = { };
          nixosModules = [
            ./machines/darktower
            # Secure secret injection
            inputs.agenix.nixosModules.default
            ./secrets/nixos.nix
          ];
          nixosSpecialArgs = {
            inherit myLib;
            inherit (inputs) agenix robotdisco-secrets;
          };
        };
      };

      apps = forAllSystems (
        system:
        let
          pkgs = nixpkgs.legacyPackages."${system}";
        in
        pkgs.lib.trivial.pipe
          [
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
              text =
                if pkgs.stdenv.isDarwin then
                  "darwin-rebuild switch --flake ${toString ./.}#"
                else
                  "sudo nixos-rebuild switch --flake ${toString ./.}#";
            }
            {
              name = "use-caches";
              runtimeInputs = [ pkgs.cachix ];
              text = ''
                ${pkgs.cachix}/bin/cachix use -O . nix-community
                ${pkgs.cachix}/bin/cachix use -O . robot-disco
              '';
            }
          ]
          [
            # Generate a derivation
            (builtins.map pkgs.writeShellApplication)
            # Transform derivation into flakes app item schema
            (builtins.map (deriv: {
              inherit (deriv) name;
              value = {
                type = "app";
                program = "${deriv}/bin/${deriv.name}";
              };
            }))
            # Convert list of app objects into attrset
            builtins.listToAttrs
          ]
      );

      devShells = forAllSystems (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          preCommitCheck = self.checks.${system}.pre-commit-check;
        in
        {
          default = pkgs.mkShell {
            inherit (preCommitCheck) shellHook;
            nativeBuildInputs = preCommitCheck.enabledPackages;
          };
        }
      );

      # Run ~nix fmt~ to use this package to format nix files
      formatter = forAllSystems (system: nixpkgs.legacyPackages."${system}".nixfmt-rfc-style);

      # Conceptually it feels like I should be defining my packages
      # in the packages settings and then defining overlays that reference
      # my flake packages. However, since I'm using the emacs overlay to
      # derive my configs and need my packages in almost every flake item to have
      # my emacs packages introduced by overlay, it was easier to define it the
      # other way around.
      overlays = {
        emacs = import ./overlays/emacs;
        default = final: _prev: { sunsama = final.callPackage ./packages/sunsama.nix { }; };
      };

      packages."x86_64-linux" =
        let
          pkgs = import nixpkgs {
            system = "x86_64-linux";
            config.allowUnfree = true;
          };
        in
        {
          sunsama = pkgs.callPackage ./packages/sunsama.nix { };
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
