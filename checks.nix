{ nixpkgs }:
let
  mkChecks =
    system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      lintTools = {
        actionlint = "${pkgs.actionlint}/bin/actionlint";
        deadnix = "${pkgs.deadnix}/bin/deadnix";
        flake-checker = "${pkgs.flake-checker}/bin/flake-checker";
        nixfmt = "${pkgs.nixfmt}/bin/nixfmt";
        statix = "${pkgs.statix}/bin/statix";
        zsh = "${pkgs.zsh}/bin/zsh";
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
              nixfmt
              statix
              zsh
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
            ${lintTools.deadnix} --exclude $(find . -name "hardware-configuration.nix" -o -name "emacs-overrides.nix") --fail

            echo "🛡️  Running Nix static analysis..."
            ${lintTools.statix} check . --ignore hardware-configuration.nix

            # 4. zsh syntax check on every file we ship to the shell
            echo "🐚 Syntax-checking zsh files..."
            find home-manager/modules/shells/files -type f -print0 \
              | while IFS= read -r -d "" f; do
                  ${lintTools.zsh} -n "$f" || exit 1
                done

            echo "✅ All lint checks passed!"
            touch $out
          '';
    };
in
{
  x86_64-linux = mkChecks "x86_64-linux";
  aarch64-darwin = mkChecks "aarch64-darwin";
}
