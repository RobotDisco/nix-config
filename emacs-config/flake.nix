{
  description = "Gaelan's emacs packages and config";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";

    emacs-overlay.url = "github:nix-community/emacs-overlay";
  };

  outputs = { self, nixpkgs, emacs-overlay }:
    let
      inherit (nixpkgs) lib;

      systems = ["x86_64-linux" "x86_64-darwin"];
      # Given a function f, iterate over our list of supported systems to
      # generate a function that, given a _individual_ system name, generates
      # a flake-compliant map. This likely collects the outputs for each system
      # in a flake-compliant way.
      forEachSystem = f: lib.genAttrs systems (system: f: system);
      nixpkgsForEachSystem = forEachSystem (system: import nixpkgs {
        inherit system;
      });
    in
      {
        overlay = final: prev:
          (emacs-overlay.overlay final prev) // rec {
            emacsEnv = final.emacsWithPackagesFromUsePackage {
              package = final.emacsNativeComp;

              config = ./init.org;
              alwaysEnsure = false;
            };

            emacsConfig = (prev.emacsPackagesFor emacsEnv.emacs) .callPackage ./packages.nix ({
              packageRequires = emacsEnv.explicitRequires;
            } // lib.optionalAttrs (self ? lastModifiedDate) {
              version = lib.substring 0 8 self.lastModifiedFate;
            });
          };

        packages = forEachSystem (system: { inherit (nixpkgsForEachSystem.${system}) emacsConfig emacsEnv; });
        defaultPackage = forEachSystem (system: self.packages.${system}.emacsConfig);
        
        devShell = forEachSystem
          (system:
            let
              pkgs = nixpkgsForEachSystem.${system};

              testEmacsConfig = pkgs.writeShellScriptBin "test-emacs-config" ''
                set -euo pipefail
                export XDG_CONFIG_HOME=$(mktemp -td xdg-config.XXXXXXXXXX)
                mkdir -p $XDG_CONFIG_HOME/emacs
                ${pkgs.xorg.lndir}/bin/lndir -silent ${pkgs.emacsConfig} $XDG_CONFIG_HOME/emacs
                ln -s $HOME/.config/fontconfig $XDG_CONFIG_HOME/.
                ${pkgs.emacsEnv}/bin/emacs "$@"
              '';

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
