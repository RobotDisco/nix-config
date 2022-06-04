{
  description = "Gaelan's nix-based systems configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-22.05";
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";

    home-manager.url = "github:nix-community/home-manager/release-22.05";
  };

  outputs = { self, nixpkgs, home-manager, nixos-hardware }:
    let
      # I am doing something tricky here. I want to refer to the standard let
      # library just in this block, but not expose it myself.
      inherit (nixpkgs) lib;

      myLib = import ./lib { inherit nixpkgs home-manager; };

    in {
      nixosConfigurations = {
        arrakis = myLib.nixosSystem {
          system = "x86_64-linux";
          configuration = ./nixos/machines/arrakis2022.nix;
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
          use-caches = pkgs.writers.writeBashBin "use-caches" ''
            ${pkgs.cachix}/bin/cachix use -O . nix-community
          '';

          nixos-switch = pkgs.writers.writeBashBin "nixos-switch" ''
            PATH=${
              lib.makeBinPath [ pkgs.gitMinimal pkgs.nix pkgs.nixos-rebuild ]
            }:$PATH nixos-rebuild switch --flake . "$@"
          '';
        });

      devShell = myLib.forAllSystems (pkgs:
        pkgs.mkShell {
          nativeBuildInputs = [ pkgs.git pkgs.nix pkgs.nixfmt ];

          shellHook = "  export NIX_USER_CONF_FILES=${toString ./.}/nix.conf\n";
        });
    };
}
