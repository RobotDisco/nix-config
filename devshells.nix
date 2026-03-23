{
  nixpkgs,
  inputs,
}:
let
  mkShells =
    system:
    let
      pkgs = import nixpkgs {
        inherit system;
        overlays = [ inputs.agenix-rekey.overlays.default ];
      };
    in
    {
      default = pkgs.mkShell {
        packages = with pkgs; [
          # Secrets management
          agenix-rekey

          # Linting and formatting tools
          actionlint
          deadnix
          flake-checker
          nil
          nixfmt
          statix

          # Task runner
          just

          # Development helper scripts

          (writeShellScriptBin "build-arrakis" ''
            echo "Building arrakis (NixOS laptop)..."
            nix build \
              .#nixosConfigurations.arrakis.config.system.build.toplevel \
              --no-link
            echo "✅ arrakis build successful!"
          '')

          (writeShellScriptBin "build-darktower" ''
            echo "Building darktower (NixOS server)..."
            nix build \
              .#nixosConfigurations.darktower.config.system.build.toplevel \
              --no-link
            echo "✅ darktower build successful!"
          '')

          (writeShellScriptBin "build-test" ''
            echo "🔨 Testing all system builds..."
            build-arrakis
            build-darktower
            ${
              if stdenv.isDarwin then
                ''
                  echo "Building fountain-of-ahmed-iii (macOS)..."
                  nix build \
                    .#darwinConfigurations.fountain-of-ahmed-iii.system \
                    --no-link
                ''
              else
                ""
            }
            echo "✅ All system builds successful!"
          '')

          (writeShellScriptBin "test-switch" ''
            echo "🧪 Test switching system configuration..."
            ${
              if stdenv.isDarwin then
                ''
                  echo "Running darwin-rebuild with rollback safety..."
                  sudo darwin-rebuild switch --flake . --rollback
                ''
              else
                ''
                  echo "Running nixos-rebuild test (no permanent changes)..."
                  sudo nixos-rebuild test --flake .
                ''
            }
            echo "✅ Test switch completed!"
          '')

          (writeShellScriptBin "apply" ''
            echo "🚀 Applying system configuration..."
            read -p "Are you sure you want to apply? (y/N) " -n 1 -r
            echo
            if [[ $REPLY =~ ^[Yy]$ ]]; then
              ${
                if stdenv.isDarwin then
                  "sudo darwin-rebuild switch --flake ."
                else
                  "sudo nixos-rebuild switch --flake ."
              }
              echo "✅ Configuration applied successfully!"
            else
              echo "❌ Apply cancelled"
              exit 1
            fi
          '')

          (writeShellScriptBin "apply-darktower" ''
            echo "Deploying to darktower via SSH..."
            sudo nixos-rebuild switch --flake .#darktower \
              --target-host darktower \
              --build-host localhost \
              --use-remote-sudo
            echo "✅ darktower deployment successful!"
          '')

          (writeShellScriptBin "lint" ''
            echo "🔍 Running all linting checks..."
            nix build .#checks.${system}.lint-check --no-link
            echo "✅ All linting checks passed!"
          '')

          (writeShellScriptBin "fmt" ''
            echo "📝 Formatting all Nix files..."
            find . -name "*.nix" ! -name "hardware-configuration.nix" \
              -exec nixfmt {} \;
            echo "✅ All files formatted!"
          '')

          (writeShellScriptBin "check" ''
            echo "🔍 Running flake checks..."
            nix flake check
            echo "✅ Flake checks passed!"
          '')

          (writeShellScriptBin "update" ''
            echo "📦 Updating flake inputs..."
            nix flake update --commit-lock-file
            echo "✅ Flake inputs updated and committed!"
          '')

          (writeShellScriptBin "dev-help" ''
            echo "🔧 Available development commands:"
            echo ""
            echo "Build verification:"
            echo "  build-arrakis    Build arrakis (NixOS laptop)"
            echo "  build-darktower  Build darktower (NixOS server)"
            echo "  build-test       Build all NixOS systems"
            echo ""
            echo "Local apply (run on arrakis):"
            echo "  test-switch      Test config, no permanent change"
            echo "  apply            Apply config permanently (prompts)"
            echo ""
            echo "Remote deploy:"
            echo "  apply-darktower  Deploy to darktower via SSH"
            echo ""
            echo "Home-manager fast iteration (run 'just --list' for commands):"
            echo "  build-home <host>    Build home-manager config (no activation)"
            echo "  inspect-home <host>  Build and link ./result for inspection"
            echo "  switch-home <host>   Apply home-manager config (rollback available)"
            echo "  Hosts: arrakis, fountain-of-ahmed-iii"
            echo ""
            echo "Emacs fast iteration (run 'just --list' for commands):"
            echo "  tangle           Tangle init.org without a full rebuild"
            echo "  emacs-dev        Test tangled config in isolation"
            echo "  emacs-dev-package  Test with a freshly built emacs binary"
            echo "  build-emacs      Build emacs package standalone"
            echo ""
            echo "Maintenance:"
            echo "  fmt              Format all Nix files"
            echo "  lint             Run all linting checks"
            echo "  check            Run flake checks"
            echo "  update           Update and commit flake inputs"
            echo "  dev-help         Show this help"
          '')
        ];

        shellHook = ''
          # Store paths for use by justfile recipes
          export EMACS_NOX="${pkgs.emacs-nox}/bin/emacs"
          export SYSTEM="${system}"

          # Set up git pre-commit hook
          if [ ! -f .git/hooks/pre-commit ]; then
            echo "Setting up git pre-commit hook..."
            cp ${./pre-commit-hook.sh} .git/hooks/pre-commit
            chmod +x .git/hooks/pre-commit
            echo "✅ Git pre-commit hook installed"
          fi

          dev-help
        '';
      };
    };
in
{
  x86_64-linux = mkShells "x86_64-linux";
  aarch64-darwin = mkShells "aarch64-darwin";
}
