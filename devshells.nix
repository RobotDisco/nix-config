{
  nixpkgs,
  inputs,
  forAllSystems,
}:

forAllSystems (
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

        # Development helper scripts
        (writeShellScriptBin "lint" ''
          echo "🔍 Running all linting checks..."
          nix build .#checks.${system}.lint-check --no-link
          echo "✅ All linting checks passed!"
        '')

        (writeShellScriptBin "fmt" ''
          echo "📝 Formatting all Nix files..."
          find . -name "*.nix" ! -name "hardware-configuration.nix" -exec nixfmt {} \;
          echo "✅ All files formatted!"
        '')

        (writeShellScriptBin "check" ''
          echo "🔍 Running flake checks..."
          nix flake check
          echo "✅ Flake checks passed!"
        '')

        (writeShellScriptBin "build-test" ''
          echo "🔨 Testing all system builds..."
          echo "Building arrakis (NixOS)..."
          nix build .#nixosConfigurations.arrakis.config.system.build.toplevel --no-link
          echo "Building darktower (NixOS)..."
          nix build .#nixosConfigurations.darktower.config.system.build.toplevel --no-link
          ${
            if stdenv.isDarwin then
              ''
                echo "Building fountain-of-ahmed-iii (macOS)..."
                nix build .#darwinConfigurations.fountain-of-ahmed-iii.system --no-link
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
          read -p "Are you sure you want to apply the configuration? (y/N) " -n 1 -r
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

        (writeShellScriptBin "update" ''
          echo "📦 Updating flake inputs..."
          nix flake update --commit-lock-file
          echo "✅ Flake inputs updated and committed!"
        '')

        (writeShellScriptBin "dev-help" ''
          echo "🔧 Available development commands:"
          echo ""
          echo "  lint        - Run all linting checks"
          echo "  fmt         - Format all Nix files"
          echo "  check       - Run flake checks"
          echo "  build-test  - Test build all system configurations"
          echo "  test-switch - Test system switch (safe, can rollback)"
          echo "  apply       - Apply system configuration (permanent)"
          echo "  update      - Update and commit flake inputs"
          echo ""
          echo "Development workflow:"
          echo "  1. Edit configs"
          echo "  2. fmt (format files)"
          echo "  3. lint (check for issues)"
          echo "  4. build-test (verify builds)"
          echo "  5. test-switch (test runtime)"
          echo "  6. apply (when confident)"
        '')
      ];

      shellHook = ''
        # Set up git pre-commit hook
        if [ ! -f .git/hooks/pre-commit ]; then
          echo "Setting up git pre-commit hook..."
          cp ${./pre-commit-hook.sh} .git/hooks/pre-commit
          chmod +x .git/hooks/pre-commit
          echo "✅ Git pre-commit hook installed"
        fi

        echo "🔧 Development environment loaded"
        echo ""
        echo "🚀 Quick commands:"
        echo "  dev-help    - Show all available commands"
        echo "  lint        - Run all linting checks"
        echo "  fmt         - Format all Nix files"
        echo "  build-test  - Test build all configurations"
        echo "  apply       - Apply system configuration"
        echo ""
        echo "Run 'dev-help' for full command list and workflow guide"
      '';
    };
  }
)
