#!/usr/bin/env bash
set -e

echo "🔍 Running pre-commit checks..."

# Get staged files by type
staged_nix_files=$(git diff --cached --name-only --diff-filter=ACM | grep '\.nix$' | grep -v hardware-configuration.nix || true)
staged_yaml_files=$(git diff --cached --name-only --diff-filter=ACM | grep -E '\.(yml|yaml)$' | grep '\.github/workflows/' || true)
staged_flake_files=$(git diff --cached --name-only --diff-filter=ACM | grep -E '^flake\.(nix|lock)$' || true)

# Exit early if no relevant files changed
if [ -z "$staged_nix_files" ] && [ -z "$staged_yaml_files" ] && [ -z "$staged_flake_files" ]; then
  echo "No relevant files staged, skipping checks"
  exit 0
fi

# 1. Format check first (fastest, most likely to fail)
if [ -n "$staged_nix_files" ]; then
  echo "📝 Checking Nix formatting..."
  echo "$staged_nix_files" | xargs nixfmt --check
fi

# 2. Syntax/structure checks (fast)
if [ -n "$staged_yaml_files" ]; then
  echo "⚡ Checking GitHub Actions syntax..."
  echo "$staged_yaml_files" | xargs actionlint
fi

if [ -n "$staged_flake_files" ]; then
  echo "🔒 Checking flake compatibility..."
  flake-checker flake.lock
fi

# 3. Static analysis (slower but thorough)
if [ -n "$staged_nix_files" ]; then
  echo "🔍 Checking for unused Nix inputs..."
  echo "$staged_nix_files" | xargs deadnix --fail

  echo "🛡️  Running Nix static analysis..."
  for file in $staged_nix_files; do
    statix check "$file"
  done
fi

echo "✅ All pre-commit checks passed!"