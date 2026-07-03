# uConsole Code-Kata Emacs Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add a lean, self-contained `emacs-kata` package (aarch64-linux only) plus a home-manager profile that turns a ClockworkPi uConsole into a focused code-kata device, without touching the existing desktop Emacs config.

**Architecture:** A new `packages/emacs-kata/` directory holds hand-written `early-init.el` + `init.el` (no org, no tangle) and a `default.nix` that calls `emacsWithPackagesFromUsePackage { package = emacs-pgtk; config = ./init.el; }`. The package is exposed only for `aarch64-linux`. A barebones `home-manager/profiles/gaelan-kata.nix` installs it plus the kata toolchains and deploys the two `.el` files verbatim via `xdg.configFile`. Because the kata Emacs is launched interactively (no daemon), `$PATH` is populated and there is no store-path substitution.

**Tech Stack:** Nix flakes, emacs-overlay (`emacsWithPackagesFromUsePackage`), Emacs 30 + `use-package`, home-manager, `just`.

## Global Constraints

- **Target system:** `emacs-kata` builds for **`aarch64-linux` only**. It must not appear in the `x86_64-linux` / `aarch64-darwin` package sets. It cannot be *built* on the dev machine (aarch64-darwin) — only *evaluated*; the real build happens on the device or an aarch64 builder.
- **Do not modify** `packages/emacs/` (the desktop config) in any way.
- **No literate/tangle layer:** `early-init.el` and `init.el` are hand-written source. There is no `.org`, no `org-babel-tangle`.
- **No store-path substitution:** packages that shell out call **bare binaries** (`rg`, `zprint`, `elm-format`, `shellcheck`). No `@name@` placeholders, no `substituteInPlace`, no `runCommand`.
- **Nix packages are supplied by the derivation:** every `use-package` form whose package must be installed carries `:ensure t`; built-in packages use `:ensure nil` or no `:ensure`. `alwaysEnsure = false`.
- **use-package hook style:** `use-package-hook-name-suffix` is set to `nil`, so **every `:hook` target is written with an explicit `-hook` suffix**.
- **Flake visibility:** `nix eval`/`nix build` only see **git-tracked** files. `git add` new files before any `nix eval`.
- **Nix hygiene:** run `just fmt` after editing any `.nix` file. No trailing whitespace in any file; keep lines ≤ 80 chars where practical.
- **Commit trailer:** end every commit message body with
  `Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>`.

---

### Task 1: Scaffold the `emacs-kata` package and wire it into the flake

Creates the package directory with **minimal** `.el` files (a single sentinel `use-package` form), the package derivation, and the flake/package-set wiring. The goal of this task is to prove end-to-end that the Nix builder parses a plain `.el` `config` for `:ensure` forms (spec §11 risk) **before** investing in the full config.

**Files:**
- Create: `packages/emacs-kata/early-init.el`
- Create: `packages/emacs-kata/init.el`
- Create: `packages/emacs-kata/default.nix`
- Modify: `packages/default.nix`
- Modify: `flake.nix:196`

**Interfaces:**
- Produces: flake output `packages.aarch64-linux.emacs-kata` (a derivation); package dir `packages/emacs-kata/` containing `early-init.el` + `init.el` (extended in Task 3) and `default.nix`.

- [ ] **Step 1: Create the minimal `early-init.el`**

Create `packages/emacs-kata/early-init.el`:

```elisp
;;; early-init.el --- uConsole kata early init -*- lexical-binding: t -*-
(setq inhibit-startup-screen t
      use-dialog-box nil)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
;; Nix supplies all packages; configure no archives.
(setq package-archives nil)
```

- [ ] **Step 2: Create the minimal `init.el` with a sentinel package**

Create `packages/emacs-kata/init.el`:

```elisp
;;; init.el --- uConsole code-kata Emacs configuration -*- lexical-binding: t -*-

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Packages are provided by the Nix `emacs-kata` derivation, which parses
;; this file for `:ensure t` use-package forms. Make :ensure a no-op at
;; runtime so use-package never contacts a package archive.
(eval-when-compile (require 'use-package))
(eval-and-compile
  (defun gaelan/use-package-ensure-ignore (&rest _args) t)
  (setq use-package-ensure-function #'gaelan/use-package-ensure-ignore)
  (setq use-package-always-defer t)
  (setq use-package-hook-name-suffix nil))

;; Sentinel: confirms the Nix builder parses this .el for :ensure forms.
;; Replaced by the full config in Task 3.
(use-package vertico
  :ensure t
  :defer 1
  :config (vertico-mode +1))
```

- [ ] **Step 3: Create the package derivation**

Create `packages/emacs-kata/default.nix`:

```nix
{
  emacsWithPackagesFromUsePackage,
  emacs-pgtk,
}:

# Lean Emacs for a uConsole code-kata device. aarch64-linux only; uses
# emacs-pgtk (no darwin patches needed). See
# docs/superpowers/specs/2026-07-03-uconsole-kata-emacs-design.md
emacsWithPackagesFromUsePackage {
  package = emacs-pgtk;

  # Parse the hand-written init.el for use-package :ensure forms so the
  # build supplies exactly the Elisp packages the config loads.
  config = ./init.el;

  # Respect the :ensure keyword rather than installing every use-package.
  alwaysEnsure = false;
}
```

- [ ] **Step 4: Add the gated package to `packages/default.nix`**

Replace the contents of `packages/default.nix` with:

```nix
{ pkgs }:
{
  emacs = pkgs.callPackage ./emacs { };
  mujmap = pkgs.callPackage ./mujmap.nix { };
}
// (pkgs.lib.optionalAttrs pkgs.stdenv.isLinux {
  sunsama = pkgs.callPackage ./sunsama.nix { };
})
// (pkgs.lib.optionalAttrs (pkgs.stdenv.hostPlatform.system == "aarch64-linux") {
  # Code-kata Emacs for the uConsole. aarch64-linux only.
  emacs-kata = pkgs.callPackage ./emacs-kata { };
})
```

- [ ] **Step 5: Add `aarch64-linux` to the flake `packages` systems**

In `flake.nix`, change line 196 from:

```nix
      packages = nixpkgs.lib.genAttrs [ "x86_64-linux" "aarch64-darwin" ] (
```

to:

```nix
      packages = nixpkgs.lib.genAttrs [ "x86_64-linux" "aarch64-darwin" "aarch64-linux" ] (
```

- [ ] **Step 6: Format the Nix files**

Run: `just fmt`
Expected: completes with no error; `packages/default.nix` and `flake.nix` reformatted if needed.

- [ ] **Step 7: Stage new files so the flake can see them**

Run: `git add packages/emacs-kata packages/default.nix flake.nix`
Expected: no output. (Flakes only evaluate git-tracked files — this step is required before `nix eval`.)

- [ ] **Step 8: Verify the derivation evaluates**

Run: `nix eval .#packages.aarch64-linux.emacs-kata.drvPath`
Expected: prints a `"/nix/store/…-emacs-…-with-packages…drv"` path with no evaluation error. (This forces `emacsWithPackagesFromUsePackage` to read and parse `init.el`; an eval error here means the wiring or a package name is wrong.)

- [ ] **Step 9: Verify the parser picked up the sentinel package**

Run: `nix derivation show -r .#packages.aarch64-linux.emacs-kata | grep -i vertico | head`
Expected: at least one line referencing an `…emacs-vertico…drv` input. This proves the `.el` was parsed and `vertico` was pulled in.
If empty: the `.el` parser did not read the file. Fallback — rename `init.el` to `init.org` wrapping the elisp in a single `#+begin_src emacs-lisp :tangle yes` block, set `config = ./init.org`, and re-run. (Do not proceed to Task 3 until this passes.)

- [ ] **Step 10: Commit**

```bash
git add packages/emacs-kata packages/default.nix flake.nix
git commit -m "$(cat <<'EOF'
feat(emacs-kata): scaffold lean kata Emacs package (aarch64-linux)

New packages/emacs-kata with minimal early-init.el/init.el and a
default.nix using emacsWithPackagesFromUsePackage over a plain .el.
Exposed as packages.aarch64-linux.emacs-kata only.

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>
EOF
)"
```

---

### Task 2: Add `justfile` recipes for the kata config

Adds a paren-check recipe and a local-iteration launcher, mirroring the existing `check-emacs` / `emacs-dev` recipes but with **no tangle step** (the `.el` files are the source).

**Files:**
- Modify: `justfile` (variable block near top; Emacs recipe section)

**Interfaces:**
- Consumes: `packages/emacs-kata/{early-init,init}.el` from Task 1.
- Produces: `just check-emacs-kata`, `just emacs-dev-kata`.

- [ ] **Step 1: Add path variables**

In `justfile`, immediately after the existing `emacs_init_org := …` line (line 5), add:

```just
emacs_kata_src := justfile_directory() / "packages/emacs-kata"
emacs_kata_dir := env_var_or_default("XDG_RUNTIME_DIR", "/tmp") \
    + "/emacs-kata-dev"
```

- [ ] **Step 2: Add the check and dev recipes**

In `justfile`, at the end of the `# --- Emacs ---` section (after the `build-emacs` recipe, currently line 138), add:

```just
# Check kata .el files for syntax errors (paren balance). No tangle.
check-emacs-kata:
    "$EMACS_NOX" --batch \
        --eval "(progn \
                  (find-file \"{{emacs_kata_src}}/early-init.el\") \
                  (check-parens) \
                  (find-file \"{{emacs_kata_src}}/init.el\") \
                  (check-parens) \
                  (message \"Syntax OK\"))"

# Launch a local emacs against the kata config for elisp iteration.
# Copies the .el files to a temp dir so the repo stays clean.
emacs-dev-kata *args:
    #!/usr/bin/env bash
    set -euo pipefail
    mkdir -p "{{emacs_kata_dir}}"
    cp "{{emacs_kata_src}}"/*.el "{{emacs_kata_dir}}/"
    echo "Starting emacs with kata config from {{emacs_kata_dir}}"
    echo "(~/.config/emacs/ is not affected)"
    exec emacs --init-directory "{{emacs_kata_dir}}" {{args}}
```

- [ ] **Step 3: Verify the check recipe passes on the minimal config**

Run: `just check-emacs-kata`
Expected: prints `Syntax OK` and exits 0.

- [ ] **Step 4: Commit**

```bash
git add justfile
git commit -m "$(cat <<'EOF'
chore(emacs-kata): add just check-emacs-kata and emacs-dev-kata recipes

Paren-check and local-iteration launcher for the kata config; no tangle
step since the .el files are hand-written source.

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>
EOF
)"
```

---

### Task 3: Write the full kata `early-init.el` and `init.el`

Replaces the minimal files with the complete editing spine, language support, and screen tuning. This is the substance of the config.

**Files:**
- Modify (overwrite): `packages/emacs-kata/early-init.el`
- Modify (overwrite): `packages/emacs-kata/init.el`

**Interfaces:**
- Consumes: the package derivation and recipes from Tasks 1–2.
- Produces: the deployable `early-init.el` / `init.el` referenced by the profile in Task 4.

- [ ] **Step 1: Write the full `early-init.el`**

Overwrite `packages/emacs-kata/early-init.el` with:

```elisp
;;; early-init.el --- uConsole kata early init -*- lexical-binding: t -*-

;; Maximize GC threshold during startup; restore after init completes.
(let ((normal-gc-cons-threshold gc-cons-threshold)
      (normal-gc-cons-percentage gc-cons-percentage))
  (setq gc-cons-threshold most-positive-fixnum
        gc-cons-percentage 1.0)
  (add-hook 'emacs-startup-hook
            (lambda ()
              (setq gc-cons-threshold normal-gc-cons-threshold
                    gc-cons-percentage normal-gc-cons-percentage))))

;; Disable file-name handlers during startup; restore after.
(let ((normal-file-name-handler-alist file-name-handler-alist))
  (setq file-name-handler-alist nil)
  (add-hook 'emacs-startup-hook
            (lambda ()
              (setq file-name-handler-alist
                    normal-file-name-handler-alist))))

(setq inhibit-compacting-font-caches t)
(setq frame-inhibit-implied-resize t)

(setq inhibit-startup-screen t
      use-dialog-box nil)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %0.03f seconds with %d garbage collections"
                     (float-time (time-subtract after-init-time
                                                before-init-time))
                     gcs-done)))

;; Nix supplies all packages; configure no archives.
(setq package-archives nil)

;; On the (darwin) dev machine used for config iteration, mirror the Linux
;; modifier layout. Harmless on the aarch64-linux device.
(when (eq system-type 'darwin)
  (setq mac-option-modifier 'super
        mac-command-modifier 'meta))
```

- [ ] **Step 2: Write the full `init.el` — bootstrap + UX + appearance**

Overwrite `packages/emacs-kata/init.el`, starting with:

```elisp
;;; init.el --- uConsole code-kata Emacs configuration -*- lexical-binding: t -*-

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Packages are provided by the Nix `emacs-kata` derivation, which parses
;; this file for `:ensure t` use-package forms. Make :ensure a no-op at
;; runtime so use-package never contacts a package archive. Hook names are
;; written with an explicit -hook suffix, so disable the auto-suffix.
(eval-when-compile (require 'use-package))
(eval-and-compile
  (defun gaelan/use-package-ensure-ignore (&rest _args) t)
  (setq use-package-ensure-function #'gaelan/use-package-ensure-ignore)
  (setq use-package-always-defer t)
  (setq use-package-hook-name-suffix nil))

;; Redirect scattered package files into etc/ and var/.
(use-package no-littering
  :ensure t
  :demand t)

;;; --- UX baseline ---
(setq tab-always-indent 'complete)
(delete-selection-mode +1)
(pixel-scroll-precision-mode +1)
(global-so-long-mode +1)
(setq read-process-output-max (* 1024 1024))

;;; --- Appearance ---
;; Built-in high-contrast theme; no dependency, legible on a small panel.
(load-theme 'modus-vivendi t)

;; Collapse minor-mode lighters into a single menu.
(use-package minions
  :ensure t
  :defer 1
  :config (minions-mode 1))

;; Compact segmented modeline.
(use-package mood-line
  :ensure t
  :defer 1
  :config (mood-line-mode))

(column-number-mode 1)
(line-number-mode 1)

;; Typeface. NOTE: :height is a TUNE-ON-DEVICE value — the uConsole panel
;; is ~294 DPI, so the right size can only be judged on the hardware.
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (with-selected-frame frame
              (set-face-attribute 'default nil
                                  :family "Iosevka Nerd Font Mono"
                                  :height 130))))

(use-package rainbow-delimiters
  :ensure t
  :hook ((cider-repl-mode-hook
          clojure-mode-hook
          clojurec-mode-hook
          clojurescript-mode-hook
          emacs-lisp-mode-hook
          geiser-repl-mode-hook
          ielm-mode-hook
          lisp-interaction-mode-hook
          lisp-mode-hook
          scheme-mode-hook)
         . rainbow-delimiters-mode))

;; Narrower fill column suits the 720p panel.
(use-package emacs
  :ensure nil
  :hook (prog-mode-hook . display-fill-column-indicator-mode)
  :custom
  (fill-column 72))
```

- [ ] **Step 3: Append the completion / minibuffer stack**

Append to `packages/emacs-kata/init.el`:

```elisp
;;; --- Completion / minibuffer ---
(use-package vertico
  :ensure t
  :defer 1
  :custom (vertico-preselect 'prompt)
  :config (vertico-mode +1))

(use-package vertico-directory
  :after vertico
  :ensure nil
  :bind (:map vertico-map
              ("RET"   . vertico-directory-enter)
              ("DEL"   . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay-hook . vertico-directory-tidy))

(use-package savehist
  :defer 2
  :config (savehist-mode))

(use-package recentf
  :defer 2
  :config (recentf-mode +1))

(use-package emacs
  :init
  ;; Hide M-x commands that don't work in the current mode.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p))

(use-package orderless
  :demand t
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :demand t
  :ensure t
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :config (marginalia-mode))

(use-package consult
  :ensure t
  :bind (("C-x b"   . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x p b" . consult-project-buffer)
         ("C-x r b" . consult-bookmark)
         ("C-x f"   . consult-recent-file)
         ("M-y"     . consult-yank-pop)
         ("M-g g"   . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o"   . consult-outline)
         ("M-g i"   . consult-imenu)
         ("M-g m"   . consult-mark)
         ("M-g k"   . consult-global-mark)
         ("M-g e"   . consult-compile-error)
         ("M-g f"   . consult-flycheck)
         ("M-s l"   . consult-line)
         ("M-s L"   . consult-line-multi)
         ("M-s r"   . consult-ripgrep)
         ("M-s g"   . consult-grep)
         ("M-s d"   . consult-find)
         ("M-s k"   . consult-keep-lines)
         ("M-s u"   . consult-focus-lines)
         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history))
  :custom
  ;; No custom ripgrep-args: the interactive Emacs finds `rg` on $PATH.
  (consult-narrow-key "<"))

(use-package consult-flycheck
  :ensure t
  :after (consult flycheck))

(use-package consult-eglot
  :ensure t
  :after (consult eglot)
  :bind (:map eglot-mode-map
              ("C-c e s" . consult-eglot-symbols)))

(use-package corfu
  :ensure t
  :defer 1
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.5)
  :config (global-corfu-mode))

(use-package cape
  :ensure t
  :defer 1
  :config
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file))

(use-package embark
  :ensure t
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)
         ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :ensure t
  :after (embark consult))

(use-package wgrep
  :ensure t
  :custom (wgrep-auto-save-buffer t))

(use-package which-key
  :ensure nil
  :defer 1
  :config (which-key-mode))
```

- [ ] **Step 4: Append editing, dev tooling, VCS, navigation**

Append to `packages/emacs-kata/init.el`:

```elisp
;;; --- Project management ---
(defvar gaelan/kata-roots '("~/katas")
  "Root directories under which kata projects live.")

(defun gaelan/refresh-known-projects ()
  "Re-scan `gaelan/kata-roots' and remember every project found."
  (interactive)
  (dolist (root gaelan/kata-roots)
    (project-remember-projects-under root t)))

(use-package project
  :ensure nil
  :bind ("C-x p P" . gaelan/refresh-known-projects))

;;; --- Structural editing ---
(use-package paredit
  :ensure t
  :hook ((cider-repl-mode-hook
          clojure-mode-hook
          clojurec-mode-hook
          clojurescript-mode-hook
          emacs-lisp-mode-hook
          geiser-repl-mode-hook
          ielm-mode-hook
          lisp-interaction-mode-hook
          lisp-mode-hook
          scheme-mode-hook)
         . enable-paredit-mode))

;;; --- Snippets ---
(use-package yasnippet
  :ensure t
  :defer 2
  :config (yas-global-mode))

(use-package yasnippet-snippets
  :ensure t
  :after (yasnippet))

;;; --- Software development ---
(use-package direnv
  :ensure t
  :defer 1
  :config (direnv-mode))

(use-package editorconfig
  :ensure t
  :defer 1
  :config (editorconfig-mode 1))

(use-package flycheck
  :ensure t
  :defer 2
  :config (global-flycheck-mode))

(use-package flycheck-eglot
  :ensure t
  :after (flycheck eglot)
  :config (global-flycheck-eglot-mode 1))

(use-package eglot
  :ensure nil
  :hook (elm-mode-hook . eglot-ensure))

(use-package apheleia
  :ensure t
  :config
  ;; Bare binaries — the kata Emacs runs with a populated $PATH (no daemon).
  (setf (alist-get 'zprint apheleia-formatters)
        '("zprint"))
  (setf (alist-get 'elm-format apheleia-formatters)
        '("elm-format" "--yes" "--stdin" "--output" "-"))
  (setf (alist-get 'clojure-mode apheleia-mode-alist) 'zprint)
  (setf (alist-get 'clojurec-mode apheleia-mode-alist) 'zprint)
  (setf (alist-get 'clojurescript-mode apheleia-mode-alist) 'zprint)
  (setf (alist-get 'elm-mode apheleia-mode-alist) 'elm-format)
  (apheleia-global-mode +1))

;;; --- Version control ---
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
         ("C-c g" . magit-dispatch)
         ("C-c f" . magit-file-dispatch))
  :custom
  (magit-wip-mode t))

(use-package diff-hl
  :ensure t
  :defer 1
  :config
  (global-diff-hl-mode)
  (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))

;;; --- Navigation ---
(use-package avy
  :ensure t
  :bind (("C-'" . avy-goto-char-timer)
         ("M-g j" . avy-goto-line))
  :custom (avy-timeout-seconds 0.3))
```

- [ ] **Step 5: Append language support, terminal, windows, notes**

Append to `packages/emacs-kata/init.el`:

```elisp
;;; --- Emacs Lisp ---
(use-package helpful
  :ensure t
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h x" . helpful-command)
         ("C-c C-d" . helpful-at-point)
         ("C-h F" . helpful-function)))

(use-package eros
  :ensure t
  :hook (emacs-lisp-mode-hook . eros-mode))

;;; --- Clojure / ClojureScript ---
(use-package flycheck-clj-kondo
  :ensure t
  :after (clojure-mode flycheck)
  :hook ((clojure-mode-hook
          clojurec-mode-hook
          clojurescript-mode-hook)
         . (lambda () (require 'flycheck-clj-kondo))))

(use-package clojure-mode
  :ensure t
  :mode (("\\.\\(clj\\|bb\\)\\'" . clojure-mode)
         ("\\.cljs\\'" . clojurescript-mode)
         ("\\.cljc\\'" . clojurec-mode)))

(use-package cider
  :ensure t
  :bind (("C-c M-j" . cider-jack-in)
         ("C-c M-J" . cider-jack-in-cljs)))

(use-package clojure-snippets
  :ensure t
  :after (clojure-mode yasnippet))

(use-package clj-refactor
  :ensure t
  :commands clj-refactor-mode
  :after (cider)
  :hook (cider-mode-hook . (lambda ()
                             (clj-refactor-mode 1)
                             (cljr-add-keybindings-with-prefix "C-c r"))))

;;; --- Scheme (Guile) ---
(use-package geiser
  :ensure t
  :commands (run-geiser))

(use-package geiser-guile
  :ensure t
  :after (geiser))

;;; --- Elm ---
(use-package elm-mode
  :ensure t
  :mode "\\.elm\\'"
  :custom (elm-sort-imports-on-save t))

;; zsh: handled by built-in sh-mode; flycheck uses shellcheck on $PATH.

;;; --- Terminal ---
(use-package vterm
  :ensure t
  :commands (vterm vterm-other-window))

;;; --- Window management ---
(use-package winner
  :ensure nil
  :bind (("C-c [" . winner-undo)
         ("C-c ]" . winner-redo))
  :config (winner-mode 1))

(use-package emacs
  :ensure nil
  :config (tab-bar-mode 1))

;;; --- Notes (minimal org) ---
(use-package org
  :ensure t
  :mode ("\\.org\\'" . org-mode)
  :hook (org-mode-hook . (lambda ()
                           (setq-local fill-column 80)
                           (visual-line-mode +1))))
```

- [ ] **Step 6: Verify syntax**

Run: `just check-emacs-kata`
Expected: prints `Syntax OK`. If it reports an unbalanced paren, fix the offending file before continuing.

- [ ] **Step 7: Verify the full package set resolves**

Run: `git add packages/emacs-kata && nix eval .#packages.aarch64-linux.emacs-kata.drvPath`
Expected: prints a `.drv` path with **no** evaluation error. (An error like `attribute 'X' missing` means a `:ensure t` package name doesn't exist in the emacs package set — fix the name.)

- [ ] **Step 8: Spot-check that key packages were pulled in**

Run: `nix derivation show -r .#packages.aarch64-linux.emacs-kata | grep -iE 'cider|geiser-guile|elm-mode|magit|consult|vterm' | head`
Expected: lines referencing each of those packages' `.drv` inputs, confirming the full config parsed.

- [ ] **Step 9: Commit**

```bash
git add packages/emacs-kata/early-init.el packages/emacs-kata/init.el
git commit -m "$(cat <<'EOF'
feat(emacs-kata): full kata config (editing spine, languages, tuning)

Completion stack, paredit, eglot/flycheck/apheleia, magit/diff-hl; langs
Guile/Clojure(script)/Elm/zsh/Elisp; modus-vivendi, Iosevka, fill-column
72. Bare binaries (no daemon, no store-path substitution).

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>
EOF
)"
```

---

### Task 4: Create the home-manager profile

A barebones profile that installs `emacs-kata` + kata toolchains + the Iosevka font and deploys the two `.el` files. No module, no `enable` option. This task does **not** wire the profile into a host (that is the separate, out-of-scope uConsole host effort), so it is validated by parse + format only.

**Files:**
- Create: `home-manager/profiles/gaelan-kata.nix`

**Interfaces:**
- Consumes: `packages/emacs-kata/` (package + `.el` files) from Tasks 1 & 3.
- Produces: an importable home-manager profile for the uConsole host.

- [ ] **Step 1: Write the profile**

Create `home-manager/profiles/gaelan-kata.nix`:

```nix
{ pkgs, ... }:

# Barebones home-manager profile for a uConsole code-kata device.
# Intentionally NOT a module: this is a single-machine composition with no
# behaviour that varies across profiles, so it has no `enable` option.
# The kata Emacs is launched interactively (no services.emacs daemon), so
# $PATH carries the toolchains below and the config needs no store paths.
let
  emacsKata = pkgs.callPackage ../../packages/emacs-kata { };
in
{
  home.packages = with pkgs; [
    emacsKata
    # Lisp
    guile
    clojure
    clj-kondo
    zprint
    # Elm
    elmPackages.elm
    elmPackages.elm-format
    elmPackages.elm-language-server
    # zsh linting (flycheck)
    shellcheck
    # editor-invoked tools
    ripgrep
    git
    # typeface
    nerd-fonts.iosevka
  ];

  # Deploy the hand-written config verbatim — no tangle, no substitution.
  xdg = {
    enable = true;
    configFile."emacs/init.el".source = ../../packages/emacs-kata/init.el;
    configFile."emacs/early-init.el".source =
      ../../packages/emacs-kata/early-init.el;
  };
}
```

- [ ] **Step 2: Verify it parses**

Run: `nix-instantiate --parse home-manager/profiles/gaelan-kata.nix > /dev/null && echo PARSE_OK`
Expected: prints `PARSE_OK` (pure syntax check; does not evaluate imports, which require a host context).

- [ ] **Step 3: Format**

Run: `just fmt`
Expected: completes; profile reformatted if needed.

- [ ] **Step 4: Commit**

```bash
git add home-manager/profiles/gaelan-kata.nix
git commit -m "$(cat <<'EOF'
feat(home): add barebones gaelan-kata profile for uConsole

Installs emacs-kata + kata toolchains (guile, clojure/clj-kondo/zprint,
elm trio, shellcheck, ripgrep, git) + Iosevka Nerd Font, and deploys the
kata init.el/early-init.el via xdg.configFile. No module, no daemon.

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>
EOF
)"
```

---

### Task 5: Write the kata config README

A short manual documenting the lean config's intent and what is intentionally absent versus the desktop config.

**Files:**
- Create: `packages/emacs-kata/README.org`

**Interfaces:**
- Consumes: the config from Task 3 (for accurate content).
- Produces: user-facing documentation.

- [ ] **Step 1: Write the README**

Create `packages/emacs-kata/README.org`:

```org
#+TITLE: uConsole Code-Kata Emacs
#+AUTHOR: Gaelan D'costa

* What this is
A deliberately lean Emacs for a ClockworkPi uConsole repurposed as a
code-kata device. It is a *separate, hand-written* config
(=early-init.el= + =init.el=, no org/tangle), not a variant of the
desktop =packages/emacs/init.org=. See the design spec at
=docs/superpowers/specs/2026-07-03-uconsole-kata-emacs-design.md=.

* Build & deploy
- Package: =packages.aarch64-linux.emacs-kata= (aarch64-linux only; it
  does not build on the dev machines — build it on the device or an
  aarch64 builder).
- Installed via =home-manager/profiles/gaelan-kata.nix=, which also
  installs the language toolchains and the Iosevka font, and deploys the
  two =.el= files verbatim (no substitution, no daemon).

* Iterating on the config
- =just check-emacs-kata= — paren/syntax check (no tangle).
- =just emacs-dev-kata= — launch a local Emacs against the kata config in
  an isolated init dir (=~/.config/emacs/= is untouched). Uses the local
  =emacs= binary, so language toolchains/LSP may be absent — those are
  exercised on-device.

* Languages
Guile (Geiser), Clojure/ClojureScript (CIDER, clj-kondo, zprint), Elm
(eglot → elm-language-server, elm-format), zsh (sh-mode + shellcheck),
Emacs Lisp (ielm, eros, helpful). Practice is *REPL-driven*; eglot's only
client is Elm.

* Intentionally absent (vs. desktop)
Email, Slack, JIRA, GCal, org-roam, org-agenda/GTD, ledger, PDF/EPUB/
LaTeX, kubernetes/restclient, AI assistance (claude-code/monet), forge,
spellchecking, just-mode, nix-mode, org-pomodoro. Theme is the built-in
=modus-vivendi=.

* Tune on the hardware
The default face =:height= (130) in =init.el= is a starting guess. The
uConsole panel is ~294 DPI; adjust the height on the device.
```

- [ ] **Step 2: Verify formatting/whitespace**

Run: `grep -nE ' +$' packages/emacs-kata/README.org || echo "no trailing whitespace"`
Expected: prints `no trailing whitespace`.

- [ ] **Step 3: Commit**

```bash
git add packages/emacs-kata/README.org
git commit -m "$(cat <<'EOF'
docs(emacs-kata): add README for the lean kata config

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>
EOF
)"
```

---

## Deferred to the uConsole host effort (out of scope here)

These are **not** tasks in this plan; they are the follow-on work that makes the package actually run on hardware:

- Add an `aarch64-linux` NixOS/home-manager host for the uConsole and import `home-manager/profiles/gaelan-kata.nix`.
- Build `emacs-kata` on the device (or an aarch64 builder / `binfmt`) and run the §10 smoke tests: Geiser REPL, CIDER jack-in, Elm eglot diagnostics, zsh shellcheck, Clojure/Elm format-on-save.
- Tune the face `:height` on the panel.
- Compare closure size vs. desktop `emacs` (`nix path-info -Sh`).

## Self-Review

**Spec coverage:**
- §3.1 separate config → Tasks 1 & 3. §3.2 separate dir → Task 1. §3.4 no emacs-base / desktop untouched → honored (no task touches `packages/emacs/`). §3.5 plain `.el` → Tasks 1 & 3. §3.6 no daemon → Task 4 (no `services.emacs`). §3.7 profile not module → Task 4. §3.8 languages → Task 3 Step 5. §3.9 no AI → Task 3 (omitted). §3.10 Iosevka → Tasks 3 & 4.
- §4 no store-path substitution → Task 3 (bare binaries) + Task 4 (no `runCommand`). §5 keep/drop list → Task 3. §6 languages table → Task 3 + Task 4 toolchains. §7 screen tuning → Task 3 (modus, fill-column 72, Iosevka height). §8 profile → Task 4. §9 wiring → Tasks 1 (flake+packages), 4 (profile), 2 (justfile), 5 (README). §10 validation → verification steps + deferred section. §11 `.el` parsing risk → Task 1 Steps 8–9 (with `.org` fallback).
- Default calls (§5): modus theme (Task 3 Step 2), drop forge/spell/just/nix-mode/pomodoro (absent from Task 3), all reflected.

**Placeholder scan:** No TBD/TODO; all code blocks are complete; the font `:height` is a documented tune-on-device value, not a placeholder.

**Type/name consistency:** `gaelan/kata-roots` + `gaelan/refresh-known-projects` defined and bound in the same block (Task 3 Step 4). `emacsKata` let-binding used in `home.packages` (Task 4). `emacs-kata` attribute name consistent across `packages/default.nix`, flake output, and profile `callPackage` path. `check-emacs-kata` / `emacs-dev-kata` recipe names consistent across Task 2 and the README.
