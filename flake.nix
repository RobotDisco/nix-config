{
  description = "Gaelan's nix-based systems configuration";

  inputs = {
    darwin.url = "github:lnl7/nix-darwin/master";
    darwin.inputs.nixpkgs.follows = "nixpkgs-mac";

    emacs-overlay.url = "github:nix-community/emacs-overlay";

    nixpkgs.url = "github:nixos/nixpkgs/nixos-22.05";
    nixpkgs-mac.url = "github:nixos/nixpkgs/nixpkgs-22.11-darwin";
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";

    home-manager.url = "github:nix-community/home-manager/release-22.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs =
    inputs@{ self, nixpkgs, nixpkgs-mac, darwin, emacs-overlay, home-manager, nixos-hardware }:
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
        darktower = myLib.nixosSystem {
          system = "x86_64-linux";
          modules = [ ./nixos/profiles/darktower.nix ];
        };
        arrakis = myLib.nixosSystem {
          system = "x86_64-linux";
          modules = [
            nixos-hardware.nixosModules.framework
            ./nixos/profiles/arrakis2022.nix
          ] ++ nixpkgs.lib.attrValues self.nixosModules;
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

      darwinConfigurations = {
        "Fountain-of-Ahmed-III" = darwin.lib.darwinSystem {
          system = "aarch64-darwin";
          modules = [({ pkgs, ... }: {
            # Use a custom configuration.nix location.
            # $ darwin-rebuild switch -I darwin-config=$HOME/.config/nixpkgs/darwin/configuration.nix
            # environment.darwinConfig = "$HOME/.config/nixpkgs/darwin/configuration.nix";

            # Auto upgrade nix package and the daemon service.
            services.nix-daemon.enable = true;
            # nix.package = pkgs.nix;

            # Create /etc/zshrc that loads the nix-darwin environment.
            programs.zsh.enable = true;

            # Used for backwards compatibility, please read the changelog before
            # changing.
            # $ darwin-rebuild changelog
            system.stateVersion = 4;
          })
                     home-manager.darwinModules.home-manager
                     {
                       home-manager = {
                         sharedModules = nixpkgs.lib.attrValues self.homeManagerModules;
                         useGlobalPkgs = true;
                         useUserPackages = false;
                       };
                     }
                     {
                       nixpkgs.overlays = [ inputs.emacs-overlay.overlays.default ] ++ lib.attrValues self.overlays;
                     }
                     {
                       users.users."gaelan.dcosta" = {
                         home = "/Users/gaelan.dcosta";
                       };

                       home-manager.users."gaelan.dcosta" = (import
                         ./home-manager/profiles/gaelan-work.nix).configuration;
                     }
                     {
                       homebrew.enable = true;
                       homebrew.taps = [
                         "homebrew/cask-drivers"
                       ];
                       homebrew.casks = [
                         "bitwarden"
                         "brave-browser"
                         "calibre"
                         "chef-workstation"
                         "kobo"
                         # Work already installs/updates this
                         #"mattermost";
                         "seafile-client"
                         "signal"
                         "slack"
                         "tidal"
                         "whatsapp"
                         "zotero"
                         # Stuff from drivers
                         "fujitsu-scansnap-manager-s1300"
                         "kensingtonworks"
                         "uhk-agent"
                         "yubico-authenticator"
                         "yubico-yubikey-manager"
                       ];
                     }
                     {
                       system.keyboard = {
                         enableKeyMapping = true;
                         remapCapsLockToControl = true;
                         # I want this to not affect internal keyboard
                         # swapLeftCommandAndLeftAlt = true;
                       };
                     }
                     {
                       programs.gnupg.agent.enable = true;
                       programs.gnupg.agent.enableSSHSupport = true;
                     }
                    ];
        };
      };

      devShells = myLib.forAllSystems (pkgs: {
        default = pkgs.mkShell {
          nativeBuildInputs = with pkgs; [ git nix nixfmt rnix-lsp ];
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
        default = final: prev: {
          # Would have been simpler to just derive this from pkgs/final, but
          # if this package ever gets into nixpkgs it likely should follow
          # the callPackage nix paradigm for flexibility
          okta-awscli = final.python3Packages.callPackage ./packages/okta-awscli.nix {};
        };
      };

      packages = myLib.forAllSystems
        (pkgs: {
          inherit (pkgs) gaelan-emacs gaelan-emacs-config okta-awscli;
        });
    };
}
