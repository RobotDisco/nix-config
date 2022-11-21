{
  description = "Gaelan's nix-based systems configuration";

  inputs = {
    emacs-overlay.url = "github:nix-community/emacs-overlay";

    nixpkgs.url = "github:nixos/nixpkgs/nixos-22.05";
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";

    home-manager.url = "github:nix-community/home-manager/release-22.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs =
    inputs@{ self, nixpkgs, emacs-overlay, home-manager, nixos-hardware }:
    let
      inherit (nixpkgs) lib;

      myLib = import ./lib {
        inherit inputs;

        inherit homeManagerSharedModules;
      };

      # Are there home-manager modules we will want to include?
      # home-manager calls these "sharedModules" for whatever reason.
      homeManagerSharedModules = [
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
        ++ lib.attrValues self.homeManagerModules;
    in {
      homeConfigurations = {
        gaelan-personal = myLib.homeManagerConfiguration
          (import ./home-manager/profiles/gaelan-personal.nix);
        gaelan-work = myLib.homeManagerConfiguration
          (import ./home-manager/profiles/gaelan-work.nix);
      };

      homeManagerModules = { default = import ./home-manager/modules; };

      nixosModules = { default = import ./nixos/modules; };

      nixosConfigurations = {
        darktower = lib.nixosSystem {
          system = "x86_64-linux";
          modules = [ ./nixos/profiles/darktower.nix ];
        };
        arrakis = myLib.nixosSystem {
          system = "x86_64-linux";
          configuration = ./nixos/profiles/arrakis2022.nix;
          myModules = lib.attrValues self.nixosModules;
          contribModules = [ nixos-hardware.nixosModules.framework ];
        };
      };

      apps = myLib.forAllSystems (pkgs:
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
            name = "home-switch";
            runtimeInputs = [ pkgs.home-manager ];
            text = "home-manager switch --flake ${toString ./.}#";
          }
          {
            name = "use-caches";
            runtimeInputs = [ pkgs.cachix ];
            text = ''
              cachix use -O . nix-community
            '';
          }
          {
            name = "nixos-switch";
            runtimeInputs = [ pkgs.nixos-rebuild ];
            text = "sudo nixos-rebuild switch --flake ${toString ./.}#";
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

      devShells = myLib.forAllSystems (pkgs: {
        default = pkgs.mkShell {
          nativeBuildInputs = with pkgs; [ git nix rnix-lsp ];
          shellHook = "  export NIX_USER_CONF_FILES=${toString ./.}/nix.conf\n";
        };
      });

      # Run ~nix fmt~ to use this package to format nix files
      formatter = myLib.forAllSystems (pkgs: pkgs.nixfmt);

      # Conceptually it feels like I should be defining my packages
      # in the packages settings and then defining overlays that reference
      # my flake packages. However, since I'm using the emacs overlay to
      # derive my configs and need my packages in almost every flake item to have
      # my emacs packages introduced by overlay, it was easier to define it the
      # other way around.
      overlays = {
        emacs = final: prev: import ./overlays/emacs final prev;
      };

      packages = myLib.forAllSystems (pkgs: {
        inherit (pkgs) gaelan-emacs gaelan-emacs-config;
      });
    };
}
