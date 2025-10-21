#!/usr/bin/env bash
set -e

echo "🔍 Running pre-commit checks and fixes..."

# Get staged files by type
staged_nix_files=$(git diff --cached --name-only --diff-filter=ACM | grep '\.nix$' | grep -v hardware-configuration.nix || true)
staged_yaml_files=$(git diff --cached --name-only --diff-filter=ACM | grep -E '\.(yml|yaml)$' | grep '\.github/workflows/' || true)
staged_flake_files=$(git diff --cached --name-only --diff-filter=ACM | grep -E '^flake\.(nix|lock)$' || true)

# Exit early if no relevant files changed
if [ -z "$staged_nix_files" ] && [ -z "$staged_yaml_files" ] && [ -z "$staged_flake_files" ]; then
  echo "No relevant files staged, skipping checks"
  exit 0
fi

# Track if any files were modified for re-staging
files_modified=false

# 1. Auto-fix formatting (nixfmt can fix formatting issues)
if [ -n "$staged_nix_files" ]; then
  echo "📝 Auto-fixing Nix formatting..."
  for file in $staged_nix_files; do
    if ! nixfmt --check "$file" &>/dev/null; then
      echo "  🔧 Fixing format: $file"
      nixfmt "$file"
      git add "$file"
      files_modified=true
    fi
  done
fi

# 2. Auto-fix static analysis issues (statix can fix some issues)
if [ -n "$staged_nix_files" ]; then
  echo "🛡️  Auto-fixing Nix static analysis issues..."
  for file in $staged_nix_files; do
    # Check if statix can fix issues in this file
    if statix check "$file" --format=json 2>/dev/null | jq -e '.report[] | select(.suggestion != null)' &>/dev/null; then
      echo "  🔧 Fixing static analysis issues: $file"
      statix fix "$file"
      git add "$file"
      files_modified=true
    fi
  done
fi

# 3. Auto-fix dead code (deadnix can remove unused inputs)
if [ -n "$staged_nix_files" ]; then
  echo "🔍 Auto-fixing unused Nix inputs..."
  for file in $staged_nix_files; do
    # Check if deadnix finds issues
    if deadnix --fail "$file" &>/dev/null; then
      : # No unused inputs found
    else
      echo "  🔧 Removing unused inputs: $file"
      deadnix --edit "$file"
      git add "$file"
      files_modified=true
    fi
  done
fi

# 4. Checks that cannot auto-fix (syntax validation)
if [ -n "$staged_yaml_files" ]; then
  echo "⚡ Checking GitHub Actions syntax..."
  echo "$staged_yaml_files" | xargs actionlint
fi

if [ -n "$staged_flake_files" ]; then
  echo "🔒 Checking flake compatibility..."
  flake-checker flake.lock
fi

# 5. Final validation after fixes
if [ -n "$staged_nix_files" ]; then
  echo "🔍 Final validation of Nix files..."
  for file in $staged_nix_files; do
    # Re-run checks to ensure everything passes
    nixfmt --check "$file"
    statix check "$file"
    deadnix --fail "$file"
  done
fi

if [ "$files_modified" = true ]; then
  echo "🔧 Files were automatically fixed and re-staged"
fi

echo "✅ All pre-commit checks passed!"