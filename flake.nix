{
  description = "Gaelan's nix-based systems configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-23.11";

    darwin.url = "github:lnl7/nix-darwin/master";
    darwin.inputs.nixpkgs.follows = "nixpkgs";

    emacs-overlay.url = "github:nix-community/emacs-overlay";
    emacs-overlay.inputs.nixpkgs.follows = "nixpkgs";

    nixos-hardware.url = "github:NixOS/nixos-hardware/master";

    home-manager.url = "github:nix-community/home-manager/release-23.11";
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
      # My helper functions.
      # The name is cringe but I want to make it clear what I have defined vs
      # what is actually part of a standard library.
      myLib = import ./lib { inherit inputs supportedSystems; };
      newLib = import ./newlib { inherit lib nixpkgs; };

    in {
      homeManagerModules = { default = import ./home-manager/modules; };

      nixosConfigurations = {
        darktower = myLib.nixosSystem {
          system = "x86_64-linux";
          modules = [
            ./nixos/machines/darktower.nix
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
        };
        arrakis = myLib.nixosSystem {
          system = "x86_64-linux";
          modules = [
            nixos-hardware.nixosModules.framework-11th-gen-intel
            ./nixos/machines/arrakis2022.nix
          ];
          stateVersion = "23.11";
        };
      };

      apps = newLib.forAllSystems (pkgs:
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
            name = "darwin-switch";
            text = "darwin-rebuild switch --flake ${toString ./.}#";
          }
          {
            name = "nixos-switch";
            text = "sudo nixos-rebuild switch --flake ${toString ./.}#";
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

      darwinConfigurations = {
        "Fountain-of-Ahmed-III" = myLib.darwinSystem {
          system = "aarch64-darwin";
          modules = [ ./darwin/machines/Fountain-of-Ahmed-III.nix ];
        };
      };

      devShells = newLib.forAllSystems (pkgs: {
        default = pkgs.mkShell {
          nativeBuildInputs = with pkgs; [ git nix nixfmt ];
          shellHook = "  export NIX_USER_CONF_FILES=${toString ./.}/nix.conf\n";
        };
      });

      # Run ~nix fmt~ to use this package to format nix files
      formatter = newLib.forAllSystems (pkgs: pkgs.nixfmt);

      # Conceptually it feels like I should be defining my packages
      # in the packages settings and then defining overlays that reference
      # my flake packages. However, since I'm using the emacs overlay to
      # derive my configs and need my packages in almost every flake item to have
      # my emacs packages introduced by overlay, it was easier to define it the
      # other way around.
      overlays = {
        emacs = final: prev: import ./overlays/emacs final prev;
        default = final: prev: {
          # Would have been simpler to just derive this from pkgs/final, but
          # if this package ever gets into nixpkgs it likely should follow
          # the callPackage nix paradigm for flexibility
          okta-awscli =
            final.python3Packages.callPackage ./packages/okta-awscli.nix { };
          brlaser = final.callPackage ./packages/brlaser.nix {};
        };
      };

      packages."x86_64-linux" =
        let
          pkgs = nixpkgs.legacyPackages."x86_64-linux";
        in
          {
            brlaser = pkgs.callPackage ./packages/brlaser.nix {};
            okta-awscli =
              pkgs.python3Packages.callPackage ./packages/okta-awscli.nix {};
          };
      packages."aarch64-darwin" =
        let
          pkgs = nixpkgs.legacyPackages."x86_64-linux";
        in
          {
            okta-awscli =
              pkgs.python3Packages.callPackage ./packages/okta-awscli.nix {};
          };
    };
}
