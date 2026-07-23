# Home Configuration Builder Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add a `myLib.homeConfiguration` builder, mirroring the existing
`myLib.nixosSystem` / `myLib.darwinSystem` pattern, and use it to eliminate
the hand-rolled `pkgs`/`pkgs-unstable`/`emacsTarot` duplication currently
spread across all four home-manager-consuming call sites in `flake.nix`.

**Architecture:** `lib/homeConfiguration.nix` is a new builder function with
the same shape as `lib/nixosSystem.nix`/`lib/darwinSystem.nix`: it takes the
flake's shared inputs once (`nixpkgs`, `nixpkgs-unstable`, `emacs-overlay`,
`home-manager`, `tarot-emacs`), and returns a function that takes
per-host config (`system`, `modules`, `hostName`, `extraSpecialArgs`) and
produces a `home-manager.lib.homeManagerConfiguration`. Separately,
`lib/nixosSystem.nix` and `lib/darwinSystem.nix` are extended to
auto-compute `emacsTarot` internally (the same way they already
auto-compute `pkgs-unstable`), removing the two remaining manual
`emacsTarot = inputs.tarot-emacs.packages."${system}".default;` lines from
the system-integrated home-manager call sites.

**Tech Stack:** Nix flakes, home-manager, nix-darwin, NixOS modules.

## Global Constraints

- This is a Nix config repo with no unit-test framework. "Tests" in this
  plan are `nix eval`/`nix build` invocations against real flake outputs —
  a task passes when the relevant output evaluates/builds successfully and,
  where noted, produces an unchanged rendered config.
- Follow existing repo conventions: `lib/*.nix` builder files take the
  full flake `args` set and destructure only what they need (see
  `lib/default.nix`'s comment on this pattern); `nixpkgs.config.allowUnfree
  = true` and the `emacs-overlay` + `packages/overlay.nix` overlay stack
  must remain identical to today's behavior.
- Run `just fmt` before considering any task's file changes final (repo's
  own hygiene rule, `CLAUDE.md`).
- Every step below shows exact file paths and complete code — no
  placeholders.

---

### Task 1: Auto-inject `emacsTarot` into `nixosSystem`/`darwinSystem`, add the `homeConfiguration` builder

**Files:**
- Modify: `flake.nix:59-68` (myLib construction — add `tarot-emacs`)
- Modify: `flake.nix:100-120` (`nixosConfigurations.arrakis` — drop manual `emacsTarot`)
- Modify: `flake.nix:76-98` (`darwinConfigurations.fountain-of-ahmed-iii` — drop manual `emacsTarot`)
- Modify: `lib/nixosSystem.nix` (auto-compute `emacsTarot`)
- Modify: `lib/darwinSystem.nix` (auto-compute `emacsTarot`)
- Create: `lib/homeConfiguration.nix`
- Modify: `lib/default.nix` (register the new builder)

**Interfaces:**
- Produces: `myLib.homeConfiguration :: { system, modules, hostName, extraSpecialArgs ? {} } -> home-manager configuration`, used by Task 2.
- Produces: `lib/nixosSystem.nix` and `lib/darwinSystem.nix` now inject `emacsTarot` into `home-manager.extraSpecialArgs` automatically — callers no longer need to pass it.

- [ ] **Step 1: Add `tarot-emacs` to `myLib`'s construction inputs**

In `flake.nix`, the `myLib` binding currently reads:

```nix
      myLib = import ./lib {
        inherit (nixpkgs) lib;
        inherit (inputs)
          darwin
          nixpkgs
          nixpkgs-unstable
          emacs-overlay
          home-manager
          ;
      };
```

Change it to:

```nix
      myLib = import ./lib {
        inherit (nixpkgs) lib;
        inherit (inputs)
          darwin
          nixpkgs
          nixpkgs-unstable
          emacs-overlay
          home-manager
          tarot-emacs
          ;
      };
```

- [ ] **Step 2: Auto-compute `emacsTarot` in `lib/nixosSystem.nix`**

Current relevant section:

```nix
{
  emacs-overlay,
  home-manager,
  nixpkgs,
  nixpkgs-unstable,
  ...
}:

{
  # Core system configuration
  system,
  modules,
  # Optional parameters with sensible defaults
  specialArgs ? { },
  homeModules ? [ ],
  homeSpecialArgs ? { },
}:

let
  # Compose all overlays with overrides for broken packages
  overlays = [
    # Include the community emacs overlay for latest packages
    emacs-overlay.overlays.default

    (import ../packages/overlay.nix)
  ];

  # Instantiate nixpkgs for this system
  # We know we'll need to allow unfree packages.
  pkgs-unstable = import nixpkgs-unstable {
    inherit system;
    config.allowUnfree = true;
  };
in
```

Change the argument list and `let` block to:

```nix
{
  emacs-overlay,
  home-manager,
  nixpkgs,
  nixpkgs-unstable,
  tarot-emacs,
  ...
}:

{
  # Core system configuration
  system,
  modules,
  # Optional parameters with sensible defaults
  specialArgs ? { },
  homeModules ? [ ],
  homeSpecialArgs ? { },
}:

let
  # Compose all overlays with overrides for broken packages
  overlays = [
    # Include the community emacs overlay for latest packages
    emacs-overlay.overlays.default

    (import ../packages/overlay.nix)
  ];

  # Instantiate nixpkgs for this system
  # We know we'll need to allow unfree packages.
  pkgs-unstable = import nixpkgs-unstable {
    inherit system;
    config.allowUnfree = true;
  };

  # Tarot's emacs package for whichever system we're building for.
  emacsTarot = tarot-emacs.packages.${system}.default;
in
```

Then update the `home-manager.extraSpecialArgs` block further down from:

```nix
    {
      # Supply home-manager with special arguments. Always include flake inputs.
      home-manager.extraSpecialArgs = {
        inherit pkgs-unstable;
      }
      // homeSpecialArgs;
    }
```

to:

```nix
    {
      # Supply home-manager with special arguments. Always include flake inputs.
      home-manager.extraSpecialArgs = {
        inherit pkgs-unstable emacsTarot;
      }
      // homeSpecialArgs;
    }
```

- [ ] **Step 3: Auto-compute `emacsTarot` in `lib/darwinSystem.nix`**

Apply the same change to `lib/darwinSystem.nix`. Its function header goes
from:

```nix
{
  darwin,
  emacs-overlay,
  home-manager,
  nixpkgs-unstable,
  ...
}:
```

to:

```nix
{
  darwin,
  emacs-overlay,
  home-manager,
  nixpkgs-unstable,
  tarot-emacs,
  ...
}:
```

Its `let` block goes from:

```nix
let
  # Compose all overlays with overrides for broken packages
  overlays = [
    # Include the community emacs overlay for latest packages
    emacs-overlay.overlays.default

    (import ../packages/overlay.nix)
  ];

  # Instantiate nixpkgs for this system
  # We know we'll need to allow unfree packages.
  pkgs-unstable = import nixpkgs-unstable {
    inherit system;
    config.allowUnfree = true;
  };
in
```

to:

```nix
let
  # Compose all overlays with overrides for broken packages
  overlays = [
    # Include the community emacs overlay for latest packages
    emacs-overlay.overlays.default

    (import ../packages/overlay.nix)
  ];

  # Instantiate nixpkgs for this system
  # We know we'll need to allow unfree packages.
  pkgs-unstable = import nixpkgs-unstable {
    inherit system;
    config.allowUnfree = true;
  };

  # Tarot's emacs package for whichever system we're building for.
  emacsTarot = tarot-emacs.packages.${system}.default;
in
```

And its `home-manager.extraSpecialArgs` block goes from:

```nix
    {
      # Supply home-manager with special arguments. Always include flake inputs.
      home-manager.extraSpecialArgs = {
        inherit pkgs-unstable;
      }
      // homeSpecialArgs;
    }
```

to:

```nix
    {
      # Supply home-manager with special arguments. Always include flake inputs.
      home-manager.extraSpecialArgs = {
        inherit pkgs-unstable emacsTarot;
      }
      // homeSpecialArgs;
    }
```

- [ ] **Step 4: Remove the now-redundant manual `emacsTarot` lines in `flake.nix`**

In `nixosConfigurations.arrakis`, change:

```nix
          homeSpecialArgs = {
            inherit myLib;
            inherit (inputs) agenix robotdisco-secrets;
            emacsTarot = inputs.tarot-emacs.packages."x86_64-linux".default;
          };
```

to:

```nix
          homeSpecialArgs = {
            inherit myLib;
            inherit (inputs) agenix robotdisco-secrets;
          };
```

In `darwinConfigurations.fountain-of-ahmed-iii`, change:

```nix
          homeSpecialArgs = {
            inherit myLib;
            inherit (inputs) agenix robotdisco-secrets;
            emacsTarot = inputs.tarot-emacs.packages."aarch64-darwin".default;
          };
```

to:

```nix
          homeSpecialArgs = {
            inherit myLib;
            inherit (inputs) agenix robotdisco-secrets;
          };
```

- [ ] **Step 5: Create `lib/homeConfiguration.nix`**

```nix
# Self-contained standalone home-manager configuration builder with overlay
# composition — mirrors nixosSystem.nix/darwinSystem.nix. Used for the
# `homeConfigurations` flake output, which lets `just build-home` /
# `just switch-home` iterate on home-manager config without a full
# NixOS/nix-darwin system rebuild.
{
  emacs-overlay,
  home-manager,
  nixpkgs,
  nixpkgs-unstable,
  tarot-emacs,
  ...
}:

{
  # Core configuration
  system,
  modules,
  hostName,
  # Optional parameters with sensible defaults
  extraSpecialArgs ? { },
}:

let
  # Compose all overlays with overrides for broken packages
  overlays = [
    # Include the community emacs overlay for latest packages
    emacs-overlay.overlays.default

    (import ../packages/overlay.nix)
  ];

  # Instantiate nixpkgs for this system
  # We know we'll need to allow unfree packages.
  pkgs = import nixpkgs {
    inherit system overlays;
    config.allowUnfree = true;
  };

  pkgs-unstable = import nixpkgs-unstable {
    inherit system;
    config.allowUnfree = true;
  };

  # Tarot's emacs package for whichever system we're building for.
  emacsTarot = tarot-emacs.packages.${system}.default;
in
home-manager.lib.homeManagerConfiguration {
  inherit pkgs;
  modules = [ ../home-manager/modules ] ++ modules;
  extraSpecialArgs = {
    inherit pkgs-unstable hostName emacsTarot;
  }
  // extraSpecialArgs;
}
```

- [ ] **Step 6: Register the builder in `lib/default.nix`**

Current file ends with:

```nix
  /*
    Build macOS system configuration with sensible defaults.
    See lib/darwinSystem.nix for full documentation.

    Same pattern as nixosSystem above - we pass all arguments and let
    darwinSystem.nix extract what it needs.
  */
  darwinSystem = import ./darwinSystem.nix args;
}
```

Add a third entry so it reads:

```nix
  /*
    Build macOS system configuration with sensible defaults.
    See lib/darwinSystem.nix for full documentation.

    Same pattern as nixosSystem above - we pass all arguments and let
    darwinSystem.nix extract what it needs.
  */
  darwinSystem = import ./darwinSystem.nix args;

  /*
    Build a standalone home-manager configuration with sensible defaults.
    See lib/homeConfiguration.nix for full documentation.

    Same pattern as nixosSystem/darwinSystem above - we pass all arguments
    and let homeConfiguration.nix extract what it needs.
  */
  homeConfiguration = import ./homeConfiguration.nix args;
}
```

- [ ] **Step 7: Verify the system configs still build and carry `emacsTarot`**

Run:

```bash
nix build .#nixosConfigurations.arrakis.config.system.build.toplevel --no-link
nix build .#darwinConfigurations.fountain-of-ahmed-iii.system.drvPath
```

Expected: both succeed with no eval errors (the darwin one only needs to
evaluate the `.drvPath`, not actually build, since we're cross-building
from Linux). If either fails with `error: attribute 'emacsTarot' missing`
or similar, check that `tarot-emacs` was threaded through the `myLib`
construction in Step 1 and that both `let` blocks in Steps 2/3 compute
`emacsTarot` before it's referenced.

- [ ] **Step 8: Commit**

```bash
git add flake.nix lib/nixosSystem.nix lib/darwinSystem.nix lib/homeConfiguration.nix lib/default.nix
git commit -m "refactor: auto-inject emacsTarot in system builders, add homeConfiguration builder"
```

---

### Task 2: Rewire the standalone `homeConfigurations` to use `myLib.homeConfiguration`

**Files:**
- Modify: `flake.nix:135-197` (the `homeConfigurations` attrset)

**Interfaces:**
- Consumes: `myLib.homeConfiguration :: { system, modules, hostName, extraSpecialArgs ? {} } -> <home-manager configuration>` from Task 1.

- [ ] **Step 1: Snapshot today's rendered home-manager output for both hosts, for later comparison**

```bash
nix build '.#homeConfigurations."gaelan@arrakis".activationPackage' --no-link --print-out-paths > /tmp/before-arrakis.txt
```

(Skip the fountain-of-ahmed-iii snapshot if not on macOS — it can't be
built from Linux without a remote builder; Step 4 below covers the
Linux-only verification path.)

- [ ] **Step 2: Replace the `homeConfigurations` block in `flake.nix`**

Current:

```nix
      homeConfigurations = {
        "gaelan@arrakis" =
          let
            system = "x86_64-linux";
            pkgs = import nixpkgs {
              inherit system;
              config.allowUnfree = true;
              overlays = [
                emacs-overlay.overlays.default
                (import ./packages/overlay.nix)
              ];
            };
            pkgs-unstable = import inputs.nixpkgs-unstable {
              inherit system;
              config.allowUnfree = true;
            };
          in
          inputs.home-manager.lib.homeManagerConfiguration {
            inherit pkgs;
            modules = [
              ./home-manager/modules
              ./home-manager/profiles/gaelan-personal.nix
              inputs.agenix.homeManagerModules.default
            ];
            extraSpecialArgs = {
              inherit myLib pkgs-unstable;
              inherit (inputs) robotdisco-secrets;
              emacsTarot = inputs.tarot-emacs.packages."${system}".default;
              hostName = "arrakis";
            };
          };
        "gaelan@fountain-of-ahmed-iii" =
          let
            system = "aarch64-darwin";
            pkgs = import nixpkgs {
              inherit system;
              config.allowUnfree = true;
              overlays = [
                emacs-overlay.overlays.default
                (import ./packages/overlay.nix)
              ];
            };
            pkgs-unstable = import inputs.nixpkgs-unstable {
              inherit system;
              config.allowUnfree = true;
            };
          in
          inputs.home-manager.lib.homeManagerConfiguration {
            inherit pkgs;
            modules = [
              ./home-manager/modules
              ./home-manager/profiles/gaelan-work.nix
              inputs.agenix.homeManagerModules.default
              inputs.mac-app-utils.homeManagerModules.default
            ];
            extraSpecialArgs = {
              inherit myLib pkgs-unstable;
              inherit (inputs) robotdisco-secrets;
              emacsTarot = inputs.tarot-emacs.packages."${system}".default;
              hostName = "fountain-of-ahmed-iii";
            };
          };
      };
```

Replace with:

```nix
      homeConfigurations = {
        "gaelan@arrakis" = myLib.homeConfiguration {
          system = "x86_64-linux";
          hostName = "arrakis";
          modules = [
            ./home-manager/profiles/gaelan-personal.nix
            inputs.agenix.homeManagerModules.default
          ];
          extraSpecialArgs = {
            inherit myLib;
            inherit (inputs) robotdisco-secrets;
          };
        };
        "gaelan@fountain-of-ahmed-iii" = myLib.homeConfiguration {
          system = "aarch64-darwin";
          hostName = "fountain-of-ahmed-iii";
          modules = [
            ./home-manager/profiles/gaelan-work.nix
            inputs.agenix.homeManagerModules.default
            inputs.mac-app-utils.homeManagerModules.default
          ];
          extraSpecialArgs = {
            inherit myLib;
            inherit (inputs) robotdisco-secrets;
          };
        };
      };
```

- [ ] **Step 3: Check for now-unused `flake.nix` bindings**

After this change, `emacs-overlay` may no longer be referenced directly
in the `outputs` function body outside of `myLib`'s construction and the
top-level `packages` output (grep to confirm):

```bash
grep -n "emacs-overlay" flake.nix
```

Expected: it's still used in the `myLib = import ./lib { ... }` call and
in the `packages = nixpkgs.lib.genAttrs ...` block further down — both of
those stay. If `emacs-overlay` (or any other binding) shows as unused
anywhere, `just fmt` (which runs `statix`/`deadnix` per this repo's
pre-commit hook) will catch it; no manual cleanup is expected here.

- [ ] **Step 4: Build both standalone home configs and diff against the snapshot**

```bash
nix build '.#homeConfigurations."gaelan@arrakis".activationPackage' --no-link --print-out-paths > /tmp/after-arrakis.txt
diff /tmp/before-arrakis.txt /tmp/after-arrakis.txt
```

Expected: **no output** (identical store path) — proving the refactor is
behavior-preserving for arrakis. If the path differs, build both and
`diff -r` their `home-files` outputs to find what actually changed before
proceeding.

For `gaelan@fountain-of-ahmed-iii`, since it can't be built from this
Linux machine, instead verify it evaluates cleanly:

```bash
nix eval '.#homeConfigurations."gaelan@fountain-of-ahmed-iii".activationPackage.drvPath'
```

Expected: prints a `.drv` path with no errors and no unexpected
`evaluation warning` lines (compare against the warning-free baseline
established in the prior 26.05-upgrade work).

- [ ] **Step 5: Run `just fmt` and check for a clean diff**

```bash
just fmt
git diff --stat
```

Expected: `flake.nix` (and the Task 1 files, if not yet committed) show
only the intended changes — no stray formatting churn elsewhere.

- [ ] **Step 6: Commit**

```bash
git add flake.nix
git commit -m "refactor: build standalone homeConfigurations via myLib.homeConfiguration"
```

---

## Self-Review Notes

- **Spec coverage:** Every one of the 4 manual `emacsTarot` sites named in
  the original ask is addressed: 2 removed via Task 1 (system builders now
  auto-inject it), 2 removed via Task 2 (standalone configs now delegate
  to the shared builder, which also auto-injects it). The broader
  "too much duplication in homeManagerConfiguration/nixosConfiguration/
  darwinConfiguration" complaint is addressed by collapsing each
  `homeConfigurations.*` entry from ~30 lines of hand-rolled `pkgs`/
  `pkgs-unstable`/overlay setup down to the same ~10-line shape already
  used by `nixosConfigurations`/`darwinConfigurations` entries.
- **Placeholder scan:** No TBD/TODO markers; every step shows complete,
  copy-pasteable code.
- **Type consistency:** `myLib.homeConfiguration`'s signature
  (`{ system, modules, hostName, extraSpecialArgs ? {} }`) is defined once
  in Task 1 Step 5 and used identically in both Task 2 call sites — no
  drift between `hostName` (Task 1) and any alternate name in Task 2.
