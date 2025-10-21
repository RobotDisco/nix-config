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
        nixfmt-rfc-style
        statix
      ];

      shellHook = ''
        echo "🔧 Development environment loaded"
        echo "Available tools:"
        echo "  actionlint      - GitHub Actions linting"
        echo "  deadnix         - Detect unused Nix inputs"
        echo "  flake-checker   - Check flake compatibility"
        echo "  nil             - Nix language server"
        echo "  nixfmt          - Nix formatter (RFC style)"
        echo "  statix          - Nix static analysis"
        echo "  agenix-rekey    - Secrets management"
        echo ""
        echo "Run 'nix flake check' to run all linters"
      '';
    };
  }
)
