{ inputs, supportedSystems }:

let
  inherit (inputs) self home-manager nixpkgs;

  lib = nixpkgs.lib;

  ## Could baseOverlays and baseHomeManagerModules be part of my
  ## configuration and passed into this file via the "input"
  ## pattern? Probably. If I keep changing it I'll have to pull
  ## it up.

  # What overlays do I expect all my machines to use?
  baseOverlays = [ inputs.emacs-overlay.overlays.default ]
    ++ lib.attrValues self.overlays;

  # What home manager modules do I expect all my configurations to use?
  baseHomeManagerModules = lib.attrValues self.homeManagerModules;

  # What NixOS / nix-darwin moduels do I expect all my machines to use?
  baseModules = [
    {
      # Inherit common overlays
      nixpkgs.overlays = baseOverlays;
    }
    # Note that we haven't loaded the (OS-specific) module that enables NixOS
    # or nix-darwin to include home-manager configurations. Thanks to the power
    # of lazy-evaluation, we can include home-manager configuration despite not
    # having the actual module loaded yet.
    {
      home-manager = {
        # Share the same pkgs attrset as nixos, don't create a separate one
        # for each user. If I ever define multiple users this is a potential
        # security hazard if they pull random things in via nix-env
        useGlobalPkgs = true;
        # Allow users to include their own packages via
        # NixOS' user.users.<name>.packages.
        # I am unsure the ramifications of this.
        useUserPackages = false;

        # This is Home Manager's attribute for importing _Home Manager_ modules
        sharedModules = baseHomeManagerModules;
      };
    }
  ];

  ## Could pkgsForSystem be used externally? Possibly. If so I'll want to
  ## include it in my output attrSet. Since other functions use it, I'll have to
  # either make a recursive attrSet (which is an anti-pattern) or define it in
  # a let block and inherit it in the output.
  pkgsForSystem = system:
    let
      correctNixpkgs =
        if nixpkgs.legacyPackages."${system}".stdenv.isDarwin then
          inputs.nixpkgs-mac
        else
          nixpkgs;
    in correctNixpkgs.legacyPackages."${system}";

in {
  # This is our wrapper around nixpkgs.lib.nixosSystem that includes a bunch of
  # configuration we want.
  nixosSystem =
    # The CPU architecture of the host being generated
    { system
    # NixOS modules or inline configuration
    , modules ? [ ]
      # Are there additional homeManager Modules specific to this system?
    , homeManagerModules ? [ ]
      # parameters to inject into every module
      # This is probably an anti-pattern honestly, since in theory all of my nix
      # modules shouldn't depend on the existence of non-standard params in the
      # module function definition.
    , specialArgs ? { }
    , stateVersion ? "23.05" }:
    let
      baseNixosModules = [
        # Standard hardware detection module I've seen in NiOS configurations
        nixpkgs.nixosModules.notDetected
        # Set the stateVersion
        { system.stateVersion = stateVersion; }
      ] ++ baseModules;
      # In an ideal world we'd be also including my flake's NixOS modules here
      # but I haven't gotten all my hosts to leverage those moduels yet
      # (so they have duplicate content)
      # ++ (lib.attrValues self.nixosModules);

      # If we're loading Home Manager config via NixOS modules, we have to load
      # Home Manager's nix module. While we're at it, set some global defaults.
      # NOTE: These are nix modules, not home-manager modules
      homeManagerNixosModules = [
        home-manager.nixosModules.home-manager
        { home-manager.sharedModules = homeManagerModules; }
      ];
    in lib.nixosSystem {
      inherit system specialArgs;

      modules = baseNixosModules ++ homeManagerNixosModules ++ modules;
    };

  # Our wrapper for nix-darwin configurations.
  darwinSystem =
    # The CPU architecture of the host being generated
    { system
    # NixOS modules or inline configuration
    , modules ? [ ]
      # Are there additional homeManager Modules specific to this system?
    , homeManagerModules ? [ ]
      # parameters to inject into every module
      # This is probably an anti-pattern honestly, since in theory all of my nix
      # modules shouldn't depend on the existence of non-standard params in the
      # module function definition.
    , specialArgs ? { } }:
    let
      baseDarwinModules = baseModules;

      homeManagerDarwinModules = [
        home-manager.darwinModules.home-manager
        { home-manager.sharedModules = homeManagerModules; }
      ];
    in inputs.darwin.lib.darwinSystem {
      inherit system specialArgs;

      modules = baseDarwinModules ++ homeManagerDarwinModules ++ modules;
    };

  homeManagerConfiguration = let
    # This is a function because we rely on the pkgset as provided in the
    # argument list.
    # This is a helper method because Linux and MacOS have different standard
    # homedir paths.
    homeDirectoryPrefix = pkgs:
      if pkgs.stdenv.hostPlatform.isDarwin then "/Users" else "/home";
  in { # What is the username of this configuration?
  username,
  # What are the home manager configuration (files or inline)
  # to set?
  configuration,
  # What is the platform of this configuration?
  system,
  # What is the package attrset for home manager to use?
  pkgs ? (pkgsForSystem { inherit system; }),
  # What is the home directory of this use?
  homeDirectory ? "${homeDirectoryPrefix pkgs}/${username}" }:
  home-manager.lib.homeManagerConfiguration {
    inherit configuration username homeDirectory system pkgs;

    # For whatever reason, the home-manager standalone attribute
    # is different than what the nixos module uses, I don't know
    # why.
    extraModules = baseHomeManagerModules;
  };
}
