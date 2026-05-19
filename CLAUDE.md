# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working
with this repository. See README.org for full project documentation,
workflows, and architecture overview.

# Development Workflow

Run `just --list` (after `nix develop` or direnv) for all available commands.

Key workflows:
- System changes: `just build-all` → `just test-switch` → `just apply`
  (`test-switch` is non-destructive; `apply` is permanent — confirm before running)
- Home-manager: `just build-home <host>` → `just switch-home <host>`
- Emacs: `just emacs-dev` or `just emacs-dev-package`
- Validation: `just check` (flake checks), `just lint` (all linting), `just fmt` (format Nix files)

# Architecture

## Key Patterns

**Package reference convention**: How binaries are referenced depends on
who provides the package:
- System-provided packages (NixOS modules, e.g. `hyprland`, `sway`,
  `uwsm`) → bare commands, since NixOS puts them in `$PATH`
- Home-manager managed packages (e.g. `brightnessctl`) → store paths
  (`${pkgs.brightnessctl}/bin/brightnessctl`) in service/daemon configs
  where `$PATH` may not be fully initialized; bare commands are
  acceptable in contexts where home-manager guarantees `$PATH`
  (e.g. compositor keybindings)

**Home-manager modules vs profiles**: `home-manager/modules/` contains
reusable option-based modules (all should have `enable` options).
`home-manager/profiles/` contains per-user/per-machine compositions of
those modules. `machines/<host>/` holds NixOS hardware and system
config only — not home-manager config.

**Auto-importing modules**: `myLib.scanPaths` automatically imports all
`.nix` files and directories from a path. Used in
`home-manager/modules/default.nix` to avoid manually listing modules.

**System builders**: `lib/nixosSystem.nix` and `lib/darwinSystem.nix`
handle overlay composition, home-manager integration, and specialArgs
forwarding. `lib/default.nix` forwards its full argument set to them
via `@args` so each builder extracts only what it needs.

**Custom packages**: Local package derivations live in `packages/`.
To add a new package: create `packages/<name>.nix`, then add it to
`packages/overlay.nix` (composed by both system builders and
`homeConfigurations` in `flake.nix`) and `packages/default.nix`
(for `nix build .#<name>`). Linux-only packages must be gated in
both files. If a single home-manager module exposes a `package`
option for the package, prefer setting that option there (see
`mujmap` in `email.nix`).

**Supported systems**: This flake targets exactly two systems:
`x86_64-linux` and `aarch64-darwin`. These are always written as
explicit attribute sets — no `forAllSystems` abstraction.

# Adding a New Host

1. Create `machines/<hostname>/` with `hardware-configuration.nix` and
   `default.nix` (or a single `machines/<hostname>.nix`)
2. Add to `flake.nix`:
   - NixOS: `nixosConfigurations` using `nixosSystem`
   - macOS: `darwinConfigurations` using `darwinSystem`
3. Add `homeConfigurations."gaelan@<hostname>"` for standalone
   home-manager iteration (see existing entries for the pattern)
4. Add `just build-home <host>` / `just switch-home <host>` to
   `justfile` — or rely on the existing parametric recipes

# Agentic Code Hygiene

- No trailing whitespace; no lines that are purely whitespace.
- Always run `just fmt` on `.nix` files as a last step (wraps `nixfmt`).
- Text lines should be no more than 80 characters unless unavoidable.
