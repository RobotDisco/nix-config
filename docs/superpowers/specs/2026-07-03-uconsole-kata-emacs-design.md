# uConsole Code-Kata Emacs — Design

**Date:** 2026-07-03
**Status:** Draft for review
**Author:** Gaelan D'costa (with Claude)

## 1. Problem & goal

The existing Emacs config (`packages/emacs/init.org`) is a maximalist
personal-computing environment — email, Slack, JIRA, GCal, GTD/agenda,
org-roam, ledger, PDF/EPUB/LaTeX, AI assistance. It is excellent as a
desktop editor but a poor fit for a **ClockworkPi uConsole** repurposed
as a **code-kata device**:

- The uConsole is **aarch64-linux** — a system the flake does not
  currently target (it targets exactly `x86_64-linux` and
  `aarch64-darwin`).
- It has a small **~5″ / 1280×720** high-DPI panel, a thumb keyboard +
  trackball, and runs on battery.
- A kata device wants a focused, self-contained editor: fast startup,
  minimal closure, no dependence on external services or secrets that
  won't exist on the device.

The Nix builder (`emacsWithPackagesFromUsePackage { config = ./init.org }`)
parses the **raw org text** for `use-package … :ensure t` forms to decide
which Elisp packages to compile into the closure. It does **not** expand
org `#+INCLUDE` and does **not** honor runtime `if`/`unless` gates.
Therefore a runtime "kata mode" flag on the existing file would still
compile and ship every heavy package — no disk/build savings on the very
device where they matter most.

**Goal:** a separate, lean, self-contained kata Emacs config plus a
home-manager profile that installs it and its language toolchains, so a
uConsole with Nix present becomes a focused kata practice machine.

## 2. Scope

**In scope**

- New lean hand-written config (`early-init.el` + `init.el`, no org/
  tangle) + its own package derivation, exposed as `.#emacs-kata` for
  **aarch64-linux only** (the device). It does **not**
  build on the dev systems (`x86_64-linux`, `aarch64-darwin`); the first
  packaged build happens on the device or an aarch64 builder. Config-level
  iteration on the dev machine just loads `init.el` in a local Emacs,
  which needs no aarch64 build (see §10).
- A home-manager profile that installs `emacs-kata` and the kata
  language toolchains, assuming **Nix is already present** on the device.
- `justfile` recipes for checking/iterating on the kata config.

**Out of scope (a later, separate effort)**

- A `machines/uconsole` NixOS host, bootloader/kernel/display config, or
  adding `aarch64-linux` to `nixosConfigurations`. This spec adds
  `aarch64-linux` only as a **package output**, not a system config.
- Any change to the desktop `init.org` / `packages/emacs/` config.

## 3. Key decisions (settled during brainstorming)

1. **Separate self-contained config, not a runtime flag** — because the
   Nix parser reads raw text (see §1). This also decomplects the two
   configs: the alternative (`(unless kata-mode …)` branches in one file)
   is precisely the braiding of two responsibilities into one unit that
   we want to avoid.
2. **Separate directory `packages/emacs-kata/`, not `packages/emacs/kata.org`**
   — matches the repo convention (one package dir/file under `packages/`),
   keeps each directory self-describing so `packages/emacs/CLAUDE.md`
   stays accurate, and keeps `justfile` recipes unambiguous. Co-locating
   two configs in one dir would silently turn that CLAUDE.md into a
   half-truth and overload the `init.org`-hardcoded recipes.
3. **The two configs may drift** — the kata `init.el` is a deliberate
   fork of the editing core, not a shared include. Some drift is
   acceptable and even desirable (different device, different needs). No
   shared Elisp.
4. **No shared `emacs-base`; desktop package untouched** — the kata
   package is aarch64-linux-only and uses `emacs-pgtk`, so it never
   applies the darwin patches that live in `packages/emacs/default.nix`.
   There is no duplication to factor out, so `packages/emacs/` is left
   completely unchanged (zero regression risk) and `emacs-kata/default.nix`
   simply sets `package = emacs-pgtk`.
5. **Plain `early-init.el` + `init.el`, not a literate org file** —
   `emacsWithPackagesFromUsePackage` parses `use-package` forms from a
   `.el` `config` just as it does from org, so the literate/tangle layer
   buys nothing for a barebones config. There is **no tangle step**; the
   `.el` files are hand-written source, so the desktop's "never edit
   tangled output" rule does not apply here.
6. **No Emacs daemon** — the kata Emacs is launched interactively, so the
   user's `$PATH` (with toolchains from the profile) is present. This lets
   us **drop the entire `@store-path@` substitution apparatus and the
   `runCommand` tangle/substitute step**; `consult`/`apheleia` call bare
   binaries. (The desktop needs store paths *only* because it runs as a
   PATH-less socket-activated systemd service.)
7. **A barebones profile, not a module** — `home-manager/profiles/
   gaelan-kata.nix`, with no `enable` option. Per repo convention,
   `modules/` holds reusable option-gated capabilities and `profiles/`
   holds per-machine compositions; the kata config is used by exactly one
   machine, so a module (and a toggle for an invariant) would be
   premature abstraction.
8. **Kata languages:** Guile Scheme, Clojure/ClojureScript, Elm, zsh,
   Emacs Lisp. (Dropped during brainstorming: Forth, Haskell, ML, PHP,
   Python, Ruby.)
9. **AI assistance dropped** — katas are self-practice; `claude-code.el`
   + `monet` + `websocket` are omitted.
10. **Font: Iosevka** (`Iosevka Nerd Font Mono`) — condensed advance width
    fits more readable columns on the narrow 720p panel.

## 4. Architecture

```
packages/
  emacs/                desktop config (UNCHANGED)
    init.org  default.nix  CLAUDE.md  README.org
  emacs-kata/           NEW — lean kata config
    early-init.el  init.el  default.nix  README.org
```

- `packages/emacs-kata/default.nix`: calls
  `emacsWithPackagesFromUsePackage { package = emacs-pgtk; config = ./init.el; alwaysEnsure = false; }`.
  **No `override` block** (none of the kept packages require the desktop's
  forks — claude-code/monet/org-timeblock are all dropped) and **no
  `extraEmacsPackages`** for tree-sitter (no `*-ts-mode` in the kept set);
  `vterm` builds via emacs-overlay from its `use-package` form.
- `early-init.el` / `init.el`: **hand-written** Emacs Lisp source (no
  tangle, no org). The same file `init.el` is referenced twice — by the
  package (for `use-package` discovery) and by the profile (deployed via
  `xdg.configFile`). It must stay a valid, loadable init file.

### No store-path substitution

Because the kata Emacs is launched interactively (no daemon, §3.5), the
user's `$PATH` includes the toolchains installed by the profile, so
packages that shell out (`consult` → `rg`, `apheleia` → `zprint` /
`elm-format`, `flycheck` → `shellcheck`) call **bare binaries**. There is
no `@name@` placeholder, no `substituteInPlace`, and no `runCommand`
config-assembly step — a deliberate contrast with `emacs.nix`, which
needs store paths only for its PATH-less systemd service.

## 5. Config contents (`early-init.el` / `init.el`)

### Keep (editing spine)
- **Startup/base:** GC threshold tuning, `file-name-handler` reset,
  chrome off (menu/tool/scroll bar), startup-time message,
  `package-archives nil`, `custom-file` redirect, `no-littering`,
  `tab-always-indent 'complete`, `delete-selection-mode`,
  `pixel-scroll-precision-mode`, `global-so-long-mode`,
  `read-process-output-max`.
- **Completion/minibuffer (full minad stack):** vertico
  (+vertico-directory), orderless, marginalia, consult
  (+consult-flycheck, +consult-eglot), corfu, cape, embark
  (+embark-consult, +wgrep), which-key, savehist, recentf.
  *(Dropped: consult-org-roam — no roam.)*
- **Structural editing:** paredit + rainbow-delimiters, hooked to the
  kept Lisp modes (clojure, clojurescript, emacs-lisp, scheme/geiser,
  lisp-interaction).
- **Dev core:** eglot, flycheck, flycheck-eglot, apheleia, yasnippet
  (+yasnippet-snippets), direnv/envrc, editorconfig, project.el (roots →
  `~/katas`).
- **VCS:** magit + diff-hl. *(Dropped: forge and the gpg
  `SSH_AUTH_SOCK` hack.)*
- **Navigation/windows:** avy, winner-mode, tab-bar-mode.
- **Terminal:** vterm (hosts REPLs, test runners, zsh scratch shell).
- **Notes:** plain org-mode (editing + babel eval) with **no** agenda,
  capture, roam, gtd, gcal, jira, super-agenda, timeblock, crypt.
- **Emacs Lisp:** helpful + eros (ielm built-in).

### Drop (device-irrelevant / heavy)
email (notmuch/smtp/xoauth2), Slack, JIRA, GCal, org-roam(+ui),
org-super-agenda, org-timeblock, org-crypt, citations, all GTD/agenda,
ledger, nov/epub, auctex, pdf-tools, org-noter, org-transclusion,
kubernetes/kubel, restclient, solo-rpg, claude-code.el/monet/websocket,
the kubectl modeline timer, Slack tracking, and the org-crypt/plstore
GPG key IDs.

### Default calls (baked in; flagged for veto)
- **Theme → built-in `modus-vivendi`** instead of `doom-themes` (drops a
  dependency; high-contrast, legible on a small panel).
- **Drop `forge`** — keep plain `magit` for local commits.
- **Drop spellchecking** (aspell) — code, not prose.
- **Drop `just` (just-mode/justl)** and **`nix-mode`** — add back if the
  device scripts katas with `just` or edits nix.
- **Drop `org-pomodoro`** — though a good timed-kata fit; add on request.

## 6. Language support

| Language | Editor | Interactivity | Formatter |
|---|---|---|---|
| Guile Scheme | scheme-mode | Geiser + geiser-guile (REPL) | — |
| Clojure/cljs | clojure-mode + cider + clj-refactor + clojure-snippets | CIDER (nREPL); flycheck-clj-kondo | zprint |
| Elm | elm-mode | eglot → elm-language-server | elm-format |
| zsh | sh-mode (built-in) | — | flycheck → shellcheck |
| Emacs Lisp | built-in | ielm + eros; helpful | — |

Note: with Python/Ruby removed, **eglot's only client is Elm**. The
device is overwhelmingly **REPL-driven** (Geiser, CIDER) — a good fit for
kata practice. eglot is kept as the extensible seam.

## 7. Screen tuning (5″ / 1280×720, ~294 DPI)

- Modeline: `mood-line` + `minions`, **stripped** of the kubectl and
  Slack segments; keep `column-number-mode`, `line-number-mode`.
- `fill-column` **72** (down from desktop 80) + fill-column indicator in
  prog-mode.
- Font: **Iosevka Nerd Font Mono**, starting `:height ≈ 130`.
  **This height is explicitly a tune-on-hardware value** — the uConsole's
  ~294 DPI makes point→pixel sizing device-specific; the config will call
  this out with a comment rather than pretend a blind value is correct.

## 8. Home-manager profile

A single barebones profile `home-manager/profiles/gaelan-kata.nix`
(assumes Nix present on device), **not** a module — no `enable` option,
no reusable abstraction (§3.6). It provides:

- `home.packages`: the `emacs-kata` package + toolchains — `guile`,
  `clojure` + `clj-kondo` + `zprint`, `elmPackages.elm` +
  `elmPackages.elm-format` + `elmPackages.elm-language-server`,
  `shellcheck`, `ripgrep`, `git`, and the Iosevka Nerd Font package.
- `xdg.configFile."emacs/init.el".source = ../../packages/emacs-kata/init.el;`
  and the same for `early-init.el` — deployed as static files, no
  `runCommand`, no substitution.
- **No `services.emacs`** — the device launches `emacs` interactively so
  `$PATH` is populated (this is what lets §4 drop store paths).

If interactive startup ever feels slow enough to want a daemon, that is a
follow-up decision that would reintroduce the PATH question — explicitly
out of scope here.

## 9. Flake / package wiring

1. `flake.nix:196` — add `"aarch64-linux"` to the `genAttrs` systems list
   for the `packages` output (one-line change; **no** new
   `nixosConfiguration`). This entry exists solely to expose
   `packages.aarch64-linux.emacs-kata`.
2. `packages/default.nix` — add `emacs-kata` **gated to aarch64-linux**
   (arch-specific, analogous to how `sunsama` is gated to Linux), e.g.
   inside `lib.optionalAttrs (pkgs.stdenv.hostPlatform.system ==
   "aarch64-linux")`. It intentionally does **not** appear in the
   `x86_64-linux` / `aarch64-darwin` package sets.
3. `home-manager/profiles/gaelan-kata.nix` — the barebones profile (§8),
   wired into the uConsole's home configuration (the host wiring itself
   is the later, out-of-scope effort). The profile installs the package
   via `pkgs.callPackage ../../packages/emacs-kata { }` (mirroring how
   `emacs.nix` references the desktop package), independent of the flake
   `packages` output.
4. **Building it:** the dev machines cannot build `emacs-kata` natively.
   The first packaged build runs on the uConsole itself, or via an
   aarch64 remote builder / `binfmt` emulation. (Emulated builds of
   `vterm`/cider deps are slow but functional; native-on-device or a real
   aarch64 builder is preferred.)
5. `justfile` — add `check-emacs-kata` (paren-check
   `packages/emacs-kata/{early-init,init}.el` — **no tangle**) and
   `emacs-dev-kata` (copy the `.el` files to a temp dir and launch a local
   Emacs with `--init-directory` there, for config-level iteration). There
   is intentionally no `tangle-kata` (nothing to tangle) and no
   `emacs-dev-package` equivalent (the package only builds on
   aarch64-linux).
6. `packages/emacs-kata/README.org` — short manual for the lean config
   (keybindings differ little from desktop; note what's intentionally
   absent).

## 10. Validation

**On the dev machine (config-level, no aarch64 build):**
- `just check-emacs-kata` — `init.el` byte-compiles / loads without error.
- `just emacs-dev-kata` — a local Emacs loads `init.el` in an isolated
  init dir; verify it starts clean and the completion stack, paredit, and
  mode associations behave. (LSP/REPL toolchains may be absent locally —
  that's fine; those are exercised on-device.)

**On the device / aarch64 builder (packaged):**
- `nix build .#emacs-kata` resolves and builds for `aarch64-linux`.
- Closure size of `emacs-kata` is materially smaller than the desktop
  `emacs` (sanity check via `nix path-info -Sh`).
- Smoke test on the uConsole: Geiser REPL starts, CIDER jack-in works
  against a scratch project, Elm buffer gets eglot diagnostics, zsh
  buffer gets shellcheck, format-on-save works for Clojure/Elm.
- Tune font `:height` on the panel (§7).

## 11. Risks / open questions

- **`.el` config parsing** — `emacsWithPackagesFromUsePackage` is
  expected to parse `use-package` forms from a plain `.el` `config` as it
  does from org. Confirm this early in implementation (build the package
  and check the package set resolves); fallback is a trivial one-block
  `.org` wrapper if `.el` parsing misbehaves.
- **Font height** cannot be finalized without the hardware — flagged as a
  tune-on-device value (§7).
- **zsh linting** via shellcheck is bash-oriented; it will flag some
  zsh-only constructs. Acceptable at kata scale; no strong zsh LSP exists.
- **aarch64-linux build** of the kept packages (notably `vterm`, cider's
  deps) is expected to work but cannot be verified on the dev machine
  (the package only builds on aarch64-linux). The first real build is
  on-device or via an aarch64 builder; budget time for build/emulation
  cost there.
