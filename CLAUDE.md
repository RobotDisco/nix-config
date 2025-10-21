# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This is a NixOS/nix-darwin flake configuration for managing multiple systems (NixOS on x86_64-linux and macOS via nix-darwin on aarch64-darwin). It uses home-manager for user-level configuration and agenix/agenix-rekey for secrets management.

## Common Commands

### Building and Switching

```zsh
# Apply system configuration (auto-detects platform)
nix run .#switch

# Update flake inputs and commit
nix flake update --commit-lock-file

# Update specific input
nix flake lock --update-input <input> --commit-lock-file

# Use local input during development
nix flake lock --override-input <input> path:../<input-path>
```

### Development

```zsh
# Enter development shell (includes pre-commit hooks and tools)
nix develop

# Format nix files
nix fmt

# Run pre-commit checks
nix flake check
```

### Secrets Management (agenix-rekey)

```zsh
# Rekey secrets (after editing secrets/agenix-rekey.nix)
agenix-rekey edit
agenix-rekey rekey
```

### Setup Cachix

```zsh
# Add project caches to nix.conf
nix run .#use-caches
```

## Architecture

### Flake Structure

The flake defines configurations for three hosts:
- **arrakis**: Framework 13" AMD laptop running NixOS (x86_64-linux)
- **fountain-of-ahmed-iii**: Work MacBook running nix-darwin (aarch64-darwin)
- **darktower**: Dell T20 homelab/NAS running NixOS (x86_64-linux)

### Directory Organization

- **machines/**: Per-host system configurations
  - Each host has a directory (e.g., `machines/arrakis/`) containing `hardware-configuration.nix` and `default.nix`
  - Single-file configs for simpler hosts (e.g., `machines/fountain-of-ahmed-iii.nix`)

- **modules/**: NixOS/nix-darwin system modules
  - `modules/common/`: Shared across NixOS and macOS
  - `modules/nixos/`: NixOS-specific
  - `modules/darwin/`: macOS-specific

- **home-manager/modules/**: User-level home-manager modules
  - Automatically imported for all users via `myLib.scanPaths`
  - Major modules: `wayland/`, `emacs/`, `budget/`, `development-environment.nix`, `shells.nix`

- **lib/**: Helper functions
  - `darwinSystem.nix`: Wrapper around `darwin.lib.darwinSystem` with common config
  - `nixosSystem.nix`: Wrapper around `nixpkgs.lib.nixosSystem` with common config
  - `scanPaths.nix`: Auto-imports all `.nix` files (except `default.nix`) and directories from a path

- **overlays/**: Nixpkgs overlays
  - `overlays/emacs/`: Emacs configuration overlay

- **packages/**: Custom package definitions

- **secrets/**: Secrets managed by agenix-rekey
  - `secrets/agenix-rekey.nix`: Secrets configuration
  - `secrets/rekeyed/`: Encrypted secret files

### Key Patterns

**Auto-importing modules**: The `myLib.scanPaths` function (defined in `lib/scanPaths.nix`) automatically imports all `.nix` files and directories from a given path. This is used in `home-manager/modules/default.nix` to avoid manually listing every module:

```nix
imports = [
  ../../secrets/agenix-rekey.nix
] ++ myLib.scanPaths ./.;
```

**System wrappers**: `lib/darwinSystem.nix` and `lib/nixosSystem.nix` are curried functions that provide consistent configuration across all hosts. They:
- Apply overlays automatically
- Configure home-manager with `useGlobalPkgs` and `useUserPackages`
- Make home-manager modules available to all users
- Pass through `specialArgs` for accessing custom libraries and secrets

**Overlays pattern**: The flake defines overlays that are automatically applied to all configurations via the system wrappers. The emacs-overlay is included from upstream and customized with additional overrides.

**Secrets**: Uses agenix-rekey for age-encrypted secrets. Secrets are defined in `secrets/agenix-rekey.nix` and stored encrypted in `secrets/rekeyed/`. The `agenix-rekey` output in the flake provides tooling for rekeying.

## Adding a New Host

1. Create `machines/<hostname>/` directory with `hardware-configuration.nix` and `default.nix`
   - Or create a single `machines/<hostname>.nix` for simpler configs
2. In `flake.nix`:
   - For NixOS: add to `nixosConfigurations` using the `nixosSystem` helper
   - For macOS: add to `darwinConfigurations` using the `darwinSystem` helper
3. Pass the new machine module path in `nixosModules` or `darwinModules`
4. Include necessary `specialArgs` (typically `myLib`, `agenix`, and `robotdisco-secrets`)

## Pre-commit Hooks

Pre-commit hooks are configured via `pre-commit-hooks.nix` and run automatically in the dev shell. Active hooks:
- **actionlint**: GitHub Actions linting
- **deadnix**: Detect unused function inputs
- **flake-checker**: Detect unsupported NixOS versions
- **nil**: Nix language server checks
- **nixfmt-rfc-style**: RFC-compliant formatting
- **statix**: Nix static analysis

Hardware configuration files are excluded from checks.
