{ nixpkgs, forAllSystems }:

forAllSystems (
  system:
  let
    pkgs = nixpkgs.legacyPackages.${system};
    lintTools = {
      actionlint = "${pkgs.actionlint}/bin/actionlint";
      deadnix = "${pkgs.deadnix}/bin/deadnix";
      flake-checker = "${pkgs.flake-checker}/bin/flake-checker";
      nil = "${pkgs.nil}/bin/nil";
      nixfmt = "${pkgs.nixfmt-rfc-style}/bin/nixfmt";
      statix = "${pkgs.statix}/bin/statix";
    };
  in
  {
    lint-check =
      pkgs.runCommand "lint"
        {
          nativeBuildInputs = with pkgs; [
            actionlint
            deadnix
            flake-checker
            nil
            nixfmt-rfc-style
            statix
          ];
        }
        ''
          cd ${./.}

          # 1. Format check first (fastest, most likely to fail)
          echo "📝 Checking Nix formatting..."
          ${lintTools.nixfmt} --check $(find . -name "*.nix" ! -name "hardware-configuration.nix")

          # 2. Syntax/structure checks (fast)
          echo "⚡ Checking GitHub Actions syntax..."
          ${lintTools.actionlint} .github/workflows/*.yaml

          echo "🔒 Checking flake compatibility..."
          ${lintTools.flake-checker} flake.lock

          # 3. Static analysis (slower but thorough)
          echo "🔍 Checking for unused Nix inputs..."
          ${lintTools.deadnix} --exclude $(find . -name "hardware-configuration.nix") --fail

          echo "🛡️  Running Nix static analysis..."
          ${lintTools.statix} check . --ignore hardware-configuration.nix

          echo "✅ All lint checks passed!"
          touch $out
        '';
  }
)
