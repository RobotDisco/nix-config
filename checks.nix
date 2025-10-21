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

          # GitHub Actions linting
          echo "Running actionlint..."
          ${lintTools.actionlint} .github/workflows/*.yaml

          # Check for unused function inputs
          echo "Running deadnix..."
          ${lintTools.deadnix} --exclude $(find . -name "hardware-configuration.nix") --fail

          # Check flake compatibility
          echo "Running flake-checker..."
          ${lintTools.flake-checker} flake.lock

          # Nix formatting check
          echo "Running nixfmt check..."
          ${lintTools.nixfmt} --check $(find . -name "*.nix" ! -name "hardware-configuration.nix")

          # Nix static analysis
          echo "Running statix..."
          ${lintTools.statix} check . --ignore hardware-configuration.nix

          touch $out
        '';
  }
)
