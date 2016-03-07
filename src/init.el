;; Configure package.el subsystem before initializing
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
	("melpa-stable" . "https://stable.melpa.org/packages/")
	("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Bootstrap 'use-package' if necessary
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Auto-install all packages by default
;; use-package isn't needed outside of compile-time
(setq use-package-always-ensure t)
(eval-when-compile
  (require 'use-package))
(require 'bind-key)

;; Add OSX path when run graphically
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config (exec-path-from-shell-initialize))

;; Solarized looks gross in console-mode
(use-package solarized-theme
  :if window-system
  :config (load-theme 'solarized-dark t))

(use-package smart-mode-line
  :init (setq sml/no-confirm-load-theme t)
  :config (sml/setup))

;; Adjust alt/super placement for mac keyboards
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)

;; I am a naughty lich and must be punished for using arrow keys
(use-package guru-mode
  :config (guru-global-mode +1))

;; Leverage .editorconfig files if they exist
(use-package editorconfig
  :config (editorconfig-mode +1))

;; Use ido everywhere, w/ fuzzy matching
(use-package flx-ido
  :init
  (ido-mode 1)
  (ido-everywhere 1)
  :config
  (flx-ido-mode 1)
  (setq ido-enable-flex-matching t)
  (use-package ido-ubiquitous
    :config (ido-ubiquitous-mode 1))
  (use-package smex
    :config (smex-initialize)
    :bind (("M-x" . smex)
	   ("M-X" . smex-major-mode-commands)
	   ;; The traditional M-x we all know and love
	   ("C-c C-c M-x" . execute-extended-command))))

;; A better buffer menu
(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))

;; A file tree mode
(use-package neotree
  :bind ("<f8>" . neotree-toggle))

;; Project manager (i.e. any VCS repo)
(use-package projectile
  :init
  (use-package ag)
  (use-package ggtags)
  :config
  (projectile-global-mode))

(use-package skewer-mode
  :init
  (use-package js2-mode
    :mode "\\.js\\'")
  :config
  (add-hook 'js2-mode-hook 'skewer-mode)
  (add-hook 'css-mode-hook 'skewer-css-mode)
  (add-hook 'html-mode-hook 'skewer-html-mode))

(use-package php-mode
  :mode "\\.php\\'"
  :interpreter "php"
  ;; The following leverages templates? I don't know what those are
  ;; :init (eval-after-load 'php-mode
  ;;         '(require 'php-ext))
  )

(use-package web-mode
  :mode (("\\.phtml\\'" . web-mode)
	 ("\\.tpl\\.php\\'" . web-mode)
	 ("\\.[agj]sp\'" . web-mode)
	 ("\\.as[cp]x\\'" . web-mode)
	 ("\\.erb\\'" . web-mode)
	 ("\\.mustache\\'" . web-mode)
	 ("\\.handlebars\\'" . web-mode)
	 ("\\.djhtml\\'" . web-mode)
	 ("\\.jsx\\'" . web-mode)))

(use-package rainbow-delimiters
  :config (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package magit
  :bind ("C-x g" . magit-status))

;; Auto-completion
(use-package company
  :config (add-hook 'after-init-hook 'global-company-mode))

;; Syntax checking
(use-package flycheck
  :config (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package twittering-mode)

(use-package cider)

(use-package inf-clojure
  :config
  (add-hook 'clojure-mode-hook 'inf-clojure-minor-mode))

(use-package haskell-mode
  :config
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation))

(use-package scala-mode2)

(use-package markdown-mode)

(use-package smartparens-config
  :ensure smartparens
  :config
  (show-smartparens-global-mode t)
  (add-hook 'prog-mode-hook 'turn-on-smartparens-mode)
  (add-hook 'clojure-mode-hook 'turn-on-smartparens-strict-mode)
  (sp-use-smartparens-bindings)
  (sp--populate-keymap '(("C-)" . sp-forward-slurp-sexp)
			 ("C-}" . sp-forward-barf-sexp)
			 ("C-(" . sp-backward-slurp-sexp)
			 ("C-{" . sp-backward-barf-sexp))))

;; Hippie Expand (the best expand?)
(global-set-key "\M- " 'hippie-expand)

;; Adjust text scale should operate globally
(defadvice text-scale-adjust (around all-buffers (arg) activate)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      ad-do-it)))

;; Delete trailing whitespace from lines/buffer before every save
(use-package whitespace
  :config
  (add-hook 'before-save-hook 'whitespace-cleanup)
  (whitespace-mode))

;; Disable default chrome
(tool-bar-mode -1)
(setq inhibit-startup-message t)

;; Show a bunch of useful things in the status mode line
(line-number-mode t)
(column-number-mode t)

;; enable y/n prompts instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; Avy for quick-jumping between words in a file
(use-package avy
  :config (avy-setup-default)
  :bind (("C-:" . avy-goto-char)
	 ("C-'" . avy-goto-char-2)
	 ("M-g g" . avy-goto-line)
	 ("M-g w" . avy-goto-word-1)
	 ("M-g e" . avy-goto-word-0)))

;; Ace-jump-window mode for quickly jumping between buffers
(use-package ace-window
  :bind ("M-p" . ace-window))

;; Multiple cursors
;; If multiple-cursors while searching is needed, consider phi-search
(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
	 ("C->" . mc/mark-next-like-this)
	 ("C-<" . mc/mark-previous-like-this)
	 ("C-c C-<" . mc/mark-all-like-this)))

;; enable undo-tree
(use-package undo-tree
  :config (global-undo-tree-mode))

;; Save recent files
(use-package recentf
  :config
  (recentf-mode)
  (setq recentf-max-menu-items 25))

;; http://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/
(defun smarter-move-beginning-of-line (arg)
  "Move point back to intendation or beginning of line

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first. If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(global-set-key [remap move-beginning-of-line]
		'smarter-move-beginning-of-line)

;; Revert buffers that have been changed outside emacs
(global-auto-revert-mode)

;; Save open files and window locations
(desktop-save-mode 1)

;; Random hacks of kindness