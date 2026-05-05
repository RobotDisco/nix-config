{
  nixpkgs,
}:
let
  mkShells =
    system:
    let
      pkgs = import nixpkgs {
        inherit system;
      };
    in
    {
      default = pkgs.mkShell {
        packages = with pkgs; [
          ## Linting and formatting tools
          # Lint github actions
          actionlint
          # Check for unused inputs
          deadnix
          # Are we using outdated flakes?
          flake-checker
          # Nix LSPs (nixd can complete nixpkgs, options, config)
          nixd
          # Nix formatter
          nixfmt
          # Nix static analysis
          statix

          # Task runner
          just
          just-lsp
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
            echo "Git pre-commit hook installed"
          fi

          echo "Run 'just --list' for available commands."
        '';
      };
    };
in
{
  x86_64-linux = mkShells "x86_64-linux";
  aarch64-darwin = mkShells "aarch64-darwin";
}
