{
  description = "Gaelan's emacs packages and config";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";

    emacs-overlay.url = "github:nix-community/emacs-overlay";
  };

  outputs = { self, nixpkgs, emacs-overlay }:
    let
      inherit (nixpkgs) lib;

      # These are the platforms I develop for
      systems = ["x86_64-linux" "x86_64-darwin"];

      # Given a function f, iterate over our list of supported systems to
      # generate a function that, given a _individual_ system name, generates
      # a flake-compliant map. This likely collects the outputs for each system
      # in a flake-compliant way.
      forEachSystem = f: lib.genAttrs systems (system: f system);

      # Generate nixpkgs for each platform that this flake might be run on
      nixpkgsForEachSystem = forEachSystem (system: import nixpkgs {
        inherit system;
        # See below for what we are overriding
        overlays = [ self.overlay ];
      });
    in
      {
        # These are the packages we are going to offer
        overlay = final: prev:
          (emacs-overlay.overlay final prev) // rec {
            # Generate custom emacs packages from our .emacs.d
            emacsEnv = final.emacsWithPackagesFromUsePackage {
              package = final.emacsNativeComp;

              config = ./init.org;
              alwaysEnsure = false;
            };

            emacsConfig = (prev.emacsPackagesFor emacsEnv.emacs) .callPackage ./emacs-config.nix ({
              packageRequires = emacsEnv.explicitRequires;
            } // lib.optionalAttrs (self ? lastModifiedDate) {
              version = lib.substring 0 8 self.lastModifiedDate;
            });
          };

        # These are the packages this flake offers
        packages = forEachSystem (system: { inherit (nixpkgsForEachSystem.${system}) emacsConfig emacsEnv; });
        defaultPackage = forEachSystem (system: self.packages.${system}.emacsConfig);

        # Generate a development environment for developing emacs scripts
        devShell = forEachSystem
          (system:
            let
              pkgs = nixpkgsForEachSystem.${system};

              # Handy dandy shell script
              testEmacsConfig = pkgs.writeShellScriptBin "test-emacs-config" ''
                set -euo pipefail
                export XDG_CONFIG_HOME=$(mktemp -td xdg-config.XXXXXXXXXX)
                mkdir -p $XDG_CONFIG_HOME/emacs
                ${pkgs.xorg.lndir}/bin/lndir -silent ${pkgs.emacsConfig} $XDG_CONFIG_HOME/emacs
                ln -s $HOME/.config/fontconfig $XDG_CONFIG_HOME/.
                ${pkgs.emacsEnv}/bin/emacs "$@"
              '';

              # It's more convenient to pull pre-built packages from cachix than regenerate it ourselves.
              # We should update it every so often.
              updateCaches = pkgs.writeShellScriptBin "update-caches" ''
                ${pkgs.cachix}/bin/cachix use -O . nix-community
              '';
            in pkgs.mkShell {
              nativeBuildInputs = with pkgs; [
                git
                nixpkgs-fmt

                testEmacsConfig
                updateCaches
              ];
            });
      };
}
