# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

# Overview

This is a NixOS/nix-darwin flake configuration for managing multiple systems (NixOS on x86_64-linux and macOS via nix-darwin on aarch64-darwin). It uses home-manager for user-level configuration and agenix/agenix-rekey for secrets management.

# Development Workflow

## Quick Start

```zsh
# Enter development environment
nix develop

# Show all available commands
dev-help

# Recommended workflow:
fmt          # Format files
lint         # Check for issues
build-test   # Verify all systems build
test-switch  # Test runtime safely
apply        # Apply when confident
```

## Development Commands

All commands are available after running `nix develop`:

### **Core Workflow**
- **`fmt`** - Format all Nix files
- **`lint`** - Run all linting checks
- **`check`** - Run basic flake validation
- **`build-test`** - Test build all system configurations
- **`test-switch`** - Safe runtime testing (can rollback)
- **`apply`** - Apply configuration permanently (with confirmation)

### **Maintenance**
- **`update`** - Update flake inputs and commit changes
- **`dev-help`** - Show all commands and workflow guide

### **Traditional Commands**
```zsh
# Direct nix commands (still work)
nix flake check
nix fmt
nix build .#nixosConfigurations.arrakis.config.system.build.toplevel

# Update flake inputs
nix flake update --commit-lock-file

# Use local input during development
nix flake lock --override-input <input> path:../<input-path>
```

## Secrets Management (agenix-rekey)

```zsh
# Rekey secrets (after editing secrets/agenix-rekey.nix)
agenix-rekey edit
agenix-rekey rekey
```

## Setup Cachix

```zsh
# Add project caches to nix.conf
nix run .#use-caches
```

# Architecture

## Flake Structure

The flake defines configurations for three hosts:
- **arrakis**: Framework 13" AMD laptop running NixOS (x86_64-linux)
- **fountain-of-ahmed-iii**: Work MacBook running nix-darwin (aarch64-darwin)
- **darktower**: Dell T20 homelab/NAS running NixOS (x86_64-linux)

## Directory Organization

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

- **lib/**: Helper functions and system builders
  - `default.nix`: Main library entry point that re-exports all helper functions
  - `nixosSystem.nix`: Self-contained NixOS system builder with overlay composition
  - `darwinSystem.nix`: Self-contained macOS system builder with overlay composition
  - `emacs-overrides.nix`: Handles broken emacs-overlay packages by overriding with stable versions
  - `systems.nix`: Cross-platform helper functions (`forEachSystem`, `forAllSystems`)
  - `scanPaths.nix`: Auto-imports all `.nix` files (except `default.nix`) and directories from a path

- **overlays/**: Nixpkgs overlays
  - `overlays/emacs/`: Emacs configuration overlay

- **packages/**: Custom package definitions

- **secrets/**: Secrets managed by agenix-rekey
  - `secrets/agenix-rekey.nix`: Secrets configuration
  - `secrets/rekeyed/`: Encrypted secret files

## Key Patterns

**Auto-importing modules**: The `myLib.scanPaths` function (defined in `lib/scanPaths.nix`) automatically imports all `.nix` files and directories from a given path. This is used in `home-manager/modules/default.nix` to avoid manually listing every module:

```nix
imports = [
  ../../secrets/agenix-rekey.nix
] ++ myLib.scanPaths ./.;
```

**System builders**: `lib/nixosSystem.nix` and `lib/darwinSystem.nix` are self-contained system builders that handle all configuration complexity. They:
- Compose overlays automatically (emacs-overlay + emacs-overrides + custom packages)
- Apply emacs package overrides for broken upstream packages
- Configure home-manager with `useGlobalPkgs` and `useUserPackages`
- Make home-manager modules available to all users
- Pass through `specialArgs` for accessing custom libraries and secrets

**Library argument forwarding**: The lib system uses a common Nix pattern where `lib/default.nix` accepts more arguments than it uses and forwards the complete argument set to system builders. This allows consistent argument passing without manual filtering and lets each function extract only what it needs.

**Emacs package management**: The system handles emacs packages through multiple layers:
- `emacs-overlay` provides the latest emacs packages and builds
- `lib/emacs-overrides.nix` overrides broken packages with stable nixpkgs versions
- `overlays/emacs/` contains custom emacs configuration overlays
- All layers are composed automatically in the system builders

**Secrets**: Uses agenix-rekey for age-encrypted secrets. Secrets are defined in `secrets/agenix-rekey.nix` and stored encrypted in `secrets/rekeyed/`. The `agenix-rekey` output in the flake provides tooling for rekeying.

# Adding a New Host

1. Create `machines/<hostname>/` directory with `hardware-configuration.nix` and `default.nix`
   - Or create a single `machines/<hostname>.nix` for simpler configs
2. In `flake.nix`:
   - For NixOS: add to `nixosConfigurations` using the `nixosSystem` helper
   - For macOS: add to `darwinConfigurations` using the `darwinSystem` helper
3. Pass the new machine module path in `nixosModules` or `darwinModules`
4. Include necessary `specialArgs` (typically `myLib`, `agenix`, and `robotdisco-secrets`)

# Development Tools and Linting

The development environment includes local linting and formatting tools (no external pre-commit-hooks dependency):
- **actionlint**: GitHub Actions linting
- **deadnix**: Detect unused function inputs
- **flake-checker**: Detect unsupported NixOS versions
- **nil**: Nix language server checks
- **nixfmt**: RFC-compliant formatting
- **statix**: Nix static analysis

## Pre-commit Hooks

The dev shell automatically sets up git pre-commit hooks that:
- Only run on staged files (fast)
- Check formatting before static analysis (fail fast)
- Skip checks when no relevant files are staged
- Run tools in optimal order (fastest to slowest)

## File Organization

Development configuration is split across files:
- **`devshells.nix`** - Development environment and helper scripts
- **`checks.nix`** - Linting and validation derivations
- **`pre-commit-hook.sh`** - Git pre-commit hook script

# Agentic code hygeine
- after generating content, ensure that new content does not have trailing whitespace or non-empty lines that are purely whitespace.
- after generating content, always run nixfmt on .nix files as a last step.
- Text lines should be no more than 80 characters long unless unavoidable.
