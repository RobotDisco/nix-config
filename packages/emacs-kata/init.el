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

;;; --- Project management ---
(defvar gaelan/kata-roots '("~/code" "~/katas")
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
