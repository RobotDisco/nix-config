;;; init.el --- Gaelan's Emacs config -*- lexical-binding: t; eval: (view-mode 1) -*-

(let ((minver "26.1"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))

(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
	    (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(setq user-full-name "Gaelan D'costa"
      user-mail-address "gdcosta@gmail.com")

(eval-when-compile
  (require 'cl-lib))

(defconst gaelan/*is-osx* (eq system-type 'darwin)
  "Is this operating system OSX?")
(defconst gaelan/*is-linux* (eq system-type 'gnu/linux)
  "Is this operating system Linux?")

(require 'package)

(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
	     '("org" . "https://orgmode.org/elpa/"))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package diminish
  :config (require 'diminish))

(use-package bind-key
  :config (require 'bind-key))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
;; The fringe is this small strip which sometimes indicates that line-specific information
;; about that line is available (there is an error on that line, for example.)
(fringe-mode -1)

;; Don't show Emacs' default splash screen
(setq inhibit-splash-screen t)

(column-number-mode +1)

(add-to-list 'default-frame-alist '(font . "CamingoCode-13"))

(use-package rebecca-theme
  :config
  (if (daemonp)
    ;; We need this hack because when you initialize emacs as a daemon,
    ;; no frame is created so a lot of important theme loading computations
    ;; do not get run. However, this is especially hacky because we don't
    ;; want to reload the theme from scratch on every frame creation but
    ;; that's the only hook we can do this, so our hook has to remove itself
    ;; when it is done.
    (cl-labels ((load-my-theme (frame)
			       (with-selected-frame frame
				 (load-theme 'rebecca t))
			       (remove-hook 'after-make-frame-functions #'load-my-theme)))
      (add-hook 'after-make-frame-functions #'load-my-theme))
  (load-theme 'rebecca t)))

(defalias 'yes-or-no-p 'y-or-n-p)

(setq vc-follow-symlinks t)

(global-set-key (kbd "s-u") 'revert-buffer)

(global-auto-revert-mode +1)

(use-package which-key
  :config
  (which-key-mode))

(use-package async
  :config
  (dired-async-mode))

(setq fill-column 80)

(visual-line-mode)

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize))

(setq-default mac-command-modifier 'meta)
(setq-default mac-option-modifier 'super)

(use-package helm
  ;; Add recommended keybindings as found in Thierry Volpiatto's guide
  ;; http://tuhdo.github.io/helm-intro.html
  :bind (("M-x" . helm-M-x)
	 ("C-x C-f" . helm-find-files)
	 ("C-x r b" . helm-filtered-bookmarks)
	 ("C-x C-b" . helm-mini)
	 ("M-y" . helm-show-kill-ring)
	 ("M-i" . helm-semantic-or-imenu)
	 ("M-s o" . helm-occur)
	 ("C-h SPC" . helm-all-mark-rings)
	 ("C-x c h r" . helm-register)
	 ("C-x c h g" . helm-google-suggest)
	 ("C-c h M-:" . helm-eval-expression-with-eldoc))
  :init
  ;; Turn on fuzzy matching in a bunch of places
  ;; turn it off if it is irritating or slows down searches.
  (setq-default helm-recentf-fuzzy-match t
		helm-buffers-fuzzy-matching t
		helm-locate-fuzzy-match t
		helm-M-x-fuzzy-match t
		helm-semantic-fuzzy-match t
		helm-imenu-fuzzy-match t
		helm-apropos-fuzzy-match t
		helm-lisp-fuzzy-completion t
		helm-session-fuzzy-match t
		helm-etags-select t)
  :config
  (require 'helm-config)
  (helm-mode +1)
  (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)

  ;; Add helmized history searching functionality for a variety of
  ;; interfaces: `eshell`, `shell-mode`, `minibuffer`,
  ;; using the same C-c C-l binding.
  (add-hook 'eshell-mode-hook
	    #'(lambda ()
		(define-key 'eshell-mode-map (kbd "C-c C-l") #'helm-eshell-history)))
  (add-hook 'shell-mode-hook
	    #'(lambda ()
		(define-key 'shell-mode-map (kbd "C-c C-l") #'helm-comint-input-ring)))
  (define-key minibuffer-local-map (kbd "C-c C-l") #'helm-minibuffer-history))

(use-package helm-ls-git
  :after helm
  :config
  ;; `helm-source-ls-git' must be defined manually
  ;; See https://github.com/emacs-helm/helm-ls-git/issues/34
  (setq helm-source-ls-git
	(and (memq 'helm-source-ls-git helm-ls-git-default-sources)
	     (helm-make-source "Git files" 'helm-ls-git-source
	       :fuzzy-match helm-ls-git-fuzzy-match)))
  (push 'helm-source-ls-git helm-mini-default-sources))

(use-package helm-descbinds
  :after helm
  :config
  (helm-descbinds-mode))

(use-package projectile
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))

(use-package helm-projectile
  :after helm
  :config
  (helm-projectile-on))

(use-package projectile-ripgrep
  :after projectile)

(use-package helm-rg
  :after helm)

(use-package treemacs)

(use-package treemacs-projectile
  :after projectile)

(use-package treemacs-magit
  :after magit)

(use-package flycheck
  :init
  (add-hook 'after-init-hook 'global-flycheck-mode))

(use-package yasnippet-snippets)
(use-package yasnippet
  :after yasnippet-snippets
  :config
  (yas-global-mode 1))

(use-package company
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :bind (("M-TAB" . 'company-complete)))

(use-package helm-company
  :after (helm company)
  :config
  (define-key company-mode-map (kbd "C-:") 'helm-company)
  (define-key company-active-map (kbd "C-:") 'helm-company))

(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
	 ("C->" . mc/mark-more-like-this)
	 ("C-<" . mc/mark-previous-like-this)
	 ("C-c C-<" . mc/mark-all-like-this)))

(use-package direnv
  :config
  (direnv-mode))

(use-package nix-sandbox
  :after flycheck
  :config
  ; (setq flycheck-command-wrapper-function
  ;      (lambda (command) (apply 'nix-shell-command (nix-current-sandbox) command))
  ;      flycheck-executable-find
  ;      (lambda (cmd) (nix-executable-find (nix-current-sandbox) cmd))))
  )
(use-package helm-nixos-options
  :after helm
  :if gaelan/*is-linux*
  :bind (("C-c C-S-n" . helm-nixos-options)))

(use-package company-nixos-options
  :if gaelan/*is-linux*
  :after company
  :config (add-to-list 'company-backends 'company-nixos-options))

(defconst gaelan/webdav-prefix
  (if gaelan/*is-osx*
      (file-name-as-directory "~/Seafile/emacs/")
    (file-name-as-directory "~/fallcube/emacs/"))
  "The root location of my emacs / org-mode files system")

(use-package org
  :pin org
  :init
  (setq-default org-lowest-priority ?D)
  (setq-default org-capture-templates
		`(("t" "Todo" entry (file+headline ,(concat gaelan/webdav-prefix "gtd/gtd.org") "Inbox")
		   "* TODO %?")
		  ("d" "Daily Morning Reflection" entry (function gaelan/org-journal-find-location)
		   "* %(format-time-string org-journal-time-format)Daily Morning Reflectionddfa\n** Things that will be achieved today\n     - [ ] %?\n** What am I grateful for?\n")
		  ("e" "Daily Evening Reflection" entry (function gaelan/org-journal-find-location)
		   "* %(format-time-string org-journal-time-format)Daily Evening Reflection\n** What things did I accomplish today?\n   1. %?\n** What did I learn?\n** What did I do to help my future?\n** What did I do to help others?\n")
		  ("w" "Weekly Reflection" entry (function gaelan/org-journal-find-location)
		   "* %(format-time-string org-journal-time-format)Weekly Reflection\n** What were you grateful for this week? Pick one and go deep.\n   %?\n** What were your biggest wins this week?\n** What tensions are you feeling this week? What is causing these tensions?\n** What can wait to happen this week?\n** What can you work on this week?\n** What can you learn this week?")
		  ("m" "Monthly Reflection" entry (function gaelan/org-journal-find-location)
		   "* %(format-time-string org-journal-time-format)Monthly Reflection\n** What were your biggest wins of the month?\n   - %?\n** What were you most grateful for this month?\n** What tensions have you removed this month?\n** What did you learn this month?\n** How have you grown this month?")
		  ("y" "Yearly Reflection" entry (functiona gaelan/org-journal-find-location)
		   "* %(format-time-string) org-journal-time-format)Yearly Reflection\n** What were your biggest wins of the year?\n   - %?\n** What were you most grateful for this year?\n** What tensions have you removed this year?\n** What did you learn this year?\n** How have you grown this year?")))
  (setq-default org-refile-targets
		`((,(concat gaelan/webdav-prefix "gtd/gtd.org") . (:maxlevel . 2))
		  (,(concat gaelan/webdav-prefix "gtd/someday.org") . (:level . 1))
		  (nil . (:level . 1))))
  (setq-default org-agenda-files
		`(,(concat gaelan/webdav-prefix "gtd/gtd.org")
		  ,(concat gaelan/webdav-prefix "gtd/tickler.org")
		  ,(concat gaelan/webdav-prefix "gtd/gcal/personal.org")
		  ,(concat gaelan/webdav-prefix "gtd/gcal/work.org")))
  :bind (("C-c l" . org-store-link)
	 ("C-c a" . org-agenda)
	 ("C-c c" . org-capture)))

(use-package org-roam
  :bind (:map org-roam-mode-map
	      ("C-c n l" . org-roam)
	      ("C-c n f" . org-roam-find-file)
	      ("C-c n g" . org-roam-graph-show)
	      :map org-mode-map
	      ("C-c n i" . org-roam-insert)
	      ("C-c n I" . org-roam-insert-immediate))
  :custom
  (org-roam-directory (concat gaelan/webdav-prefix "brain"))
  (org-roam-db-location (if gaelan/*is-osx*
			    (concat org-roam-directory "/db/osx.db")
			  (concat org-roam-directory "/db/linux.db")))

  (org-roam-completion-system 'helm)
  ;; I don't care about graphing daily notes or historical stuff
  (org-roam-graph-exclude-matcher '("journal"))
  (org-roam-capture-templates 
   '(("d" "default" plain (function org-roam--capture-get-point)
      "%?"
      :file-name "%<%Y%m%d%H%M%S>-${slug}"
      :head "#+title: ${title}\n"
      :unnarrowed t)
     ("f" "fleeting" plain (function org-roam--capture-get-point)
      "%?"
      :file-name "%<%Y%m%d%H%M%S>-${slug}"
      :head "#+title: ${title}\n#+roam_tags: fleeting-note\n"
      :unnarrowed t)
     ("l" "literature" plain (function org-roam--capture-get-point)
      "%?"
      :file-name "%<%Y%m%d%H%M%S>-${slug}"
      :head "#+title: ${title}\n#+roam_tags: literature-note\n"
      :unnarrowed t)))
  :config
  (add-hook 'after-init-hook 'org-roam-mode)
  ;;  org-roam-protocol is used to handle weblinks (e.g. org-roam-server)
  (require 'org-roam-protocol))

(use-package org-roam-server
  :after org-roam
  :config
  (setq org-roam-server-host "127.0.0.1"
	org-roam-server-port 8080
	org-roam-server-export-inline-images t
	org-roam-server-authenticate nil
	org-roam-server-network-poll t
	org-roam-server-network-arrows nil
	org-roam-server-network-label-truncate t
	org-roam-server-network-label-truncate-length 60
	org-roam-server-network-label-wrap-length 20))

(use-package deft
  :after org
  :bind ("C-c n d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory (concat gaelan/webdav-prefix "brain")))

(winner-mode +1)

(use-package org-journal
  :after org
  :bind ("C-c n j" . org-journal-new-entry)
  :custom
  (org-journal-date-format "%A, %F")
  (org-journal-dir (file-name-as-directory (concat gaelan/webdav-prefix "brain/" "journal")))
  (org-journal-file-format "%Y/%m/%Y-%m-%d.org"))

(defun gaelan/org-journal-find-location ()
  ;; Open today's journal, but specify a non-nil prefix argument in order to
  ;; inhibit inserting the heading; org-capture will insert the heading.
  (org-journal-new-entry t)
  ;; Position point on the journal's top-level heading so that org-capture
  ;; will add the new entry as a child entry.
  (goto-char (point-min)))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode))

(defun gaelan/org-replace-link-by-link-description ()
  "Replace an org link by its description; or if empty, its address.

   Source: https://emacs.stackexchange.com/questions/10707/in-org-mode-how-to-remove-a-link
   and modified slightly to place the url in the kill ring."
  (interactive)
  (if (org-in-regexp org-link-bracket-re 1)
      (save-excursion
	(let ((remove (list (match-beginning 0) (match-end 0)))
	      (description (if (match-end 3)
			       (org-match-string-no-properties 3)
			     (org-match-string-no-properties 1))))
	  (apply 'kill-region remove)
	  (insert description)))))

(use-package magit
  ;; I should have a keybinding that displays magit-status from anywhere
  :bind (("C-x g" . magit-status))
  :config
  ;; Enable pseudo-worktree for uncommitted files.
  (require 'magit-wip)
  (magit-wip-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  ;; Enable some built-in LSP clients
  :hook (go-mode . lsp-deferred))

(use-package lsp-ui
  :commands lsp-ui-mode
  :after lsp-mode
  :hook (lsp-mode-hook . lsp-ui-mode))

(use-package company-lsp
  :after company
  :config
  (push 'company-lsp company-backends))

(use-package lsp-treemacs
  :after lsp-mode
  :config
  (lsp-treemacs-sync-mode +1))

(use-package helm-lsp)

(use-package docker
  :bind ("C-c d" . docker))

(use-package docker-tramp)

(use-package rainbow-delimiters)

(show-paren-mode)

(use-package smartparens
  :config
  (require 'smartparens-config)
  (sp-use-smartparens-bindings))

(defun gaelan/generic-lisp-mode-hook ()
  "Mode hook when working in any Lisp."
  ;; Unlike non-lispy editing modes, we should never allow unbalanced parens
  (smartparens-strict-mode)
  ;; Enable visual disambiguation of nested parentheses
  (rainbow-delimiters-mode)
  ;; Show documentation for a function/variable in the minibuffer
  (turn-on-eldoc-mode))

(use-package slime
  :config
  (require 'slime-autoloads)
  (setq slime-contribs '(slime-repl slime-autodoc slime-references slime-scratch slime-mdot-fu slime-quicklisp))
  (setq slime-lisp-implementations '((sbcl ("sbcl" "--core" "~/code/common-lisp/sbcl.core-for-slime")))))

(setq inferior-lisp-program "sbcl")

(add-hook 'lisp-mode-hook 'gaelan/generic-lisp-mode-hook)
(add-hook 'slime-repl-mode-hook 'gaelan/generic-lisp-mode-hook)

(add-hook 'emacs-lisp-mode-hook 'gaelan/generic-lisp-mode-hook)

(define-key emacs-lisp-mode-map (kbd "C-c C-c") 'eval-defun)
(define-key emacs-lisp-mode-map (kbd "C-c C-p") 'eval-print-last-sexp)
(define-key emacs-lisp-mode-map (kbd "C-c C-r") 'eval-region)
(define-key emacs-lisp-mode-map (kbd "C-c C-k") 'eval-buffer)
(define-key emacs-lisp-mode-map (kbd "C-c C-l") 'load-file)
(define-key emacs-lisp-mode-map (kbd "C-c RET") 'macroexpand-1)
(define-key emacs-lisp-mode-map (kbd "C-c M-m") 'macroexpand-all)

(use-package clojure-mode
  :config
  (add-hook 'clojure-mode-hook #'gaelan/generic-lisp-mode-hook)
  (add-hook 'clojure-mode-hook #'subword-mode))

(use-package cider
  :config
  (add-hook 'cider-repl-mode-hook #'gaelan/generic-lisp-mode-hook)
  (add-hook 'cider-repl-mode-hook #'subword-mode))

(use-package helm-cider
  :after helm)

(defun gaelan/clj-refactor-hook ()
  (clj-refactor-mode 1)
  (yas-minor-mode 1)
  (cljr-add-keybindings-with-prefix "C-c C-m"))

(use-package clj-refactor
  :config
  (add-hook 'clojure-mode-hook #'gaelan/clj-refactor-hook))

(use-package flycheck-clj-kondo
  :after clojure-mode)

(use-package cider-eval-sexp-fu)

(use-package kaocha-runner
  :bind ((:map clojure-mode-map
	       ("C-c k t" . kaocha-runner-run-test-at-point)
	       ("C-c k r" . kaocha-runner-run-tests)
	       ("C-c k a" . kaocha-runner-run-all-tests)
	       ("C-c k w" . kaocha-runner-show-warnings)
	       ("C-c k h" . kaocha-runner-hide-windows))))

(use-package go-mode)

(use-package pyenv-mode
  :config
  (add-hook 'python-mode 'pyenv-mode))

(use-package anaconda-mode
  :config
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode))

(use-package company-anaconda
  :after company
  :config
  (add-to-list 'company-backends '(company-anaconda :with company-capf)))

(use-package lsp-haskell
  :hook (haskell-mode-hook . lsp-deferred))

(use-package terraform-mode)

(use-package company-terraform
  :after company
  :config
  (company-terraform-init))

(use-package yaml-mode)

(use-package nix-mode)

(defun gaelan/exwm-update-title-hook ()
  "EXWM hook for renaming buffer names to their associated X window title."
  (exwm-workspace-rename-buffer exwm-title))

(defun gaelan/exwm-update-class-hook ()
  "EXWM hook for renaming buffer names to their associated X window class."
  (exwm-workspace-rename-buffer exwm-class-name))

(defun gaelan/exwm-randr-screen-change-hook ()
  (call-process "autorandr" nil nil nil "--change"))

(when gaelan/*is-linux*
  (use-package exwm
    :config
    ;; Set some global window management bindings
    (setq exwm-input-global-keys
	  `(
	    ;; 's-r': Reset to (line-mode).
	    ([?\s-r] . exwm-reset)
	    ;; 's-w': Switch workspace.
	    ([?\s-w] . exwm-workspace-switch)
	    ;; 's-b': Bring application to current workspace
	    ([?\s-b] . exwm-workspace-switch-to-buffer)
	    ;; 's-p': Launch application
	    ([?\s-p] . (lambda (command)
			 (interactive (list (read-shell-command "$ ")))
			 (start-process-shell-command command nil command)))
	    ;; 's-<N>': Switch to certain workspace.
	    ,@(mapcar (lambda (i)
			`(,(kbd (format "s-%d" i)) .
			  (lambda ()
			    (interactive)
			    (exwm-workspace-switch-create ,i))))
		      (number-sequence 0 9))))
    ;; translate emacs keybindings into CUA-like ones for most apps, since most
    ;; apps don't observe emacs kebindings and we would like a uniform experience.
    (setq exwm-input-simulation-keys
	  '(;; movement
	    ([?\C-b] . [left])
	    ([?\M-b] . [C-left])
	    ([?\C-f] . [right])
	    ([?\M-f] . [C-right])
	    ([?\C-p] . [up])
	    ([?\C-n] . [down])
	    ([?\C-a] . [home])
	    ([?\C-e] . [end])
	    ([?\M-v] . [prior])
	    ([?\C-v] . [next])
	    ([?\C-d] . [delete])
	    ([?\C-k] . [S-end delete])
	    ;; cut/paste
	    ([?\C-w] . [?\C-x])
	    ([?\M-w] . [?\C-c])
	    ([?\C-y] . [?\C-v])
	    ;; search
	    ([?\C-s] . [?\C-f])))
    ;; Configure workspaces 2,3 to display  on my portrait monitor.
    ;; By default, workspaces show up on the first, default, active monitor.
    (setq exwm-randr-workspace-monitor-plist
	  '(0 "DP-1-1" 1 "DP-1-1" 2 "DP-1-2" 3 "DP-1-2"))

    ;; Pin certain applications to specific workspaces
    (setq exwm-manage-configurations
	  '(((string= exwm-class-name "Firefox") workspace 2)
	    ((string= exwm-class-name "Chromium-browser") workspace 3)
	    ((string= exwm-class-name ".obs-wrapped") workspace 2)))


    (add-hook 'exwm-update-class-hook
	      'gaelan/exwm-update-class-hook)
    (add-hook 'exwm-update-title-hook
	      'gaelan/exwm-update-title-hook)

    ;; Enable multi-monitor support for EXWM
    (require 'exwm-randr)
    (add-hook 'exwm-randr-screen-change-hook
	      'gaelan/exwm-randr-screen-change-hook)
    (exwm-randr-enable)))

(when gaelan/*is-linux*
  (use-package desktop-environment
    :config
    (desktop-environment-mode)))

(when gaelan/*is-linux*
  (use-package helm-exwm
    :init
    (setq-default helm-source-names-using-follow '("EXWM buffers"))
    :config
    (setq helm-exwm-emacs-buffers-source (helm-exwm-build-emacs-buffers-source))
    (setq helm-exwm-source (helm-exwm-build-source))
    (push 'helm-exwm-emacs-buffers-source helm-mini-default-sources)
    (push 'helm-exwm-source helm-mini-default-sources)))
