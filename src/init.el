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

;; Use Anonymous Pro as default font
(add-to-list 'default-frame-alist
	     '(font . "Anonymous Pro-14"))

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
  (use-package ido-completing-read+
    :config (ido-ubiquitous-mode 1)))

;; Make M-x leverage ido
(use-package smex
    :config (smex-initialize)
    :bind (("M-x" . smex)
	   ("M-X" . smex-major-mode-commands)
	   ;; The traditional M-x we all know and love
	   ("C-c C-c M-x" . execute-extended-command)))

;; Show available key bindings
(use-package which-key
  :config
  (which-key-mode))

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
  (projectile-mode))

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

;; Ruby
(use-package rbenv
  :config (add-hook 'ruby-mode-hook #'rbenv-mode))
(use-package ruby-tools)

;; Common Lisp
;; (load (expand-file-name "~/.quicklisp/slime-helper.el"))
;; Replace "sbcl" with the path to your implementation
;; (setq inferior-lisp-program "sbcl")
;; (setq slime-contribs '(slime-fancy))

;; Clojure
(use-package cider)
(use-package clj-refactor
  :config
  (defun clj-refactor-mode-hook ()
    (clj-refactor-mode 1)
    (yas-minor-mode 1)
    ;; This choice of keybinding leaves cider-macroexpand-1 unbound
    (cljr-add-keybindings-with-prefix "C-c C-m"))
  (setq cider-cljs-lein-repl
      "(do (require 'figwheel-sidecar.repl-api)
	   (figwheel-sidecar.repl-api/start-figwheel!)
	   (figwheel-sidecar.repl-api/cljs-repl))")
  (add-hook 'clojure-mode-hook #'clj-refactor-mode-hook))

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

;; function to edit init file
(defun edit-init-file ()
  "Open the init file."
  (interactive)
  (find-file user-init-file))

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

;; enable undo-tree
;; (use-package undo-tree
;;  :config (global-undo-tree-mode))

(use-package anaconda-mode
  :config
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
  (use-package company-anaconda
    :config
    (eval-after-load "company"
      '(add-to-list 'company-backends 'company-anaconda))))

(use-package pyenv-mode
  :config
  (add-hook 'python-mode-hook 'pyenv-mode))

;; Save recent files
(use-package recentf
  :config
  (recentf-mode)
  (setq recentf-max-menu-items 25))

;; Web mode
(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)))

;; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'web-mode)

;; Modern JavaScript
(use-package js2-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-jsx-mode))
  (add-to-list 'interpreter-mode-alist '("node" . js2-jsx-mode)))

(use-package json-mode
  :config (add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode)))

(use-package flycheck-flow
  :config
  (add-hook 'javascript-mode-hook 'flycheck-mode)
  (flycheck-add-next-checker 'javascript-flow 'javascript-flow-coverage 'javascript-eslint)
  (eval-after-load 'company
    '(add-to-list 'company-backends 'company-flow)))

(use-package flow-minor-mode
  :config
  (add-hook 'js2-mode-hook 'flow-minor-enable-automatically)
  (flycheck-add-mode 'javascript-flow 'flow-minor-mode)
  (flycheck-add-mode 'javascript-eslint 'flow-minor-mode))

;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(javascript-jshint)))

(let ((gaelan-webdav-prefix (if (eql system-type 'darwin)
			     (file-name-as-directory "/Volumes/webdav/")
			   (file-name-as-directory "~/webdav/"))))
  ;; Org mode
  (use-package org
    :config
    (global-set-key "\C-cl" 'org-store-link)
    (global-set-key "\C-ca" 'org-agenda)
    (global-set-key "\C-cc" 'org-capture)
    (global-set-key "\C-cb" 'org-iswitchb)
    (setq org-directory (file-name-as-directory (concat gaelan-webdav-prefix "gtd")))
    (setq org-mobile-directory (file-name-as-directory (concat org-directory "mobileorg")))
    (setq org-mobile-inbox-for-pull (concat org-directory "inbox.org"))
    (setq org-agenda-files (list (concat org-directory "personal-gtd.org")
				 (concat org-directory "work-gtd.org")
				 (concat org-directory "personal-tickler.org")
				 (concat org-directory "work-tickler.org")))
    (setq org-capture-templates `(("i" "Inbox" entry
				   (file ,(concat org-directory "inbox.org"))
				   "* TODO %i%?")
				  ("p" "Personal Tickler" entry
				   (file ,(concat org-directory "personal-tickler.org"))
				   "* %i%? \n %U")
				  ("w" "Work Tickler" entry
				   (file ,(concat org-directory "work-tickler.org"))
				   "* %i%? \n %U")))
    (setq org-refile-targets `((,(concat org-directory "personal-gtd.org") :maxlevel . 3)
			       (,(concat org-directory "work-gtd.org") :maxlevel . 3)
			       (,(concat org-directory "personal-someday.org") :level . 1)
			       (,(concat org-directory "work-someday.org") :level . 1)
			       (,(concat org-directory "personal-tickler.org") :maxlevel . 2)
			       (,(concat org-directory "work-tickler.org") :maxlevel . 2)))
    (setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
    (setq org-tag-alist '((:startgroup . nil) ("@home" . ?h) ("@officeto" . ?t) ("@officekw" . ?k) ("@phone" . ?p) ("@lappy" . ?l) ("@online" . ?i) ("@email" . ?e) ("@errand" . ?r) ("@waitingfor" . ?w) (:endgroup . nil))))

  (use-package org-journal
    :config
    (setq org-journal-dir (file-name-as-directory (concat gaelan-webdav-prefix "journal")))))


;; http://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/
(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation or beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
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

;; Random hacks of kindness
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (json-mode flycheck-flow company-flow flow-minor-mode 4clojure js2-mode yaml-mode which-key web-mode use-package undo-tree solarized-theme smex smartparens smart-mode-line ruby-tools robe rbenv rainbow-delimiters projectile notmuch neotree markdown-mode magit macrostep inf-clojure ido-ubiquitous guru-mode ggtags flycheck flx-ido exec-path-from-shell editorconfig cyberpunk-theme company-anaconda clj-refactor ag))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
