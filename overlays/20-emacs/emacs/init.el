;;; init.el --- Gaelan's Emacs config -*- lexical-binding: t; eval: (view-mode 1) -*-

(let ((minver "27.1"))
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

(defconst gaelan/*is-osx* (eq system-type 'darwin))
(defconst gaelan/*is-linux* (eq system-type 'gnu/linux))

;; Font faces
(defvar gaelan/default-font-face "CamingoCode")
(defvar gaelan/default-variable-font-face "Lato")

;; Font sizes, divide by 10 to get point size.
(defvar gaelan/default-font-size 130)
(defvar gaelan/default-variable-font-size 130)

;; Make frame transparency overridable
(defvar gaelan/frame-transparency '(90 . 90))

(require 'package)

(setq package-quickstart t)

(eval-when-compile
  (require 'use-package))

(eval-and-compile
  (setq use-package-always-defer t))

(use-package diminish
  :ensure t)

(use-package bind-key
  :ensure t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Don't show Emacs' default splash screen
(setq inhibit-splash-screen t)

;; Set frame transparency
(set-frame-parameter (selected-frame) 'alpha gaelan/frame-transparency)
(add-to-list 'default-frame-alist `(alpha . ,gaelan/frame-transparency))
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(column-number-mode +1)

(set-fringe-mode 10)

(setq visual-bell t)

(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                vterm-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(defun gaelan/set-font-faces ()
  (set-face-attribute 'default nil :font gaelan/default-font-face :height gaelan/default-font-size)
  ;; Set the fixed font face and height
  (set-face-attribute 'fixed-pitch nil :font gaelan/default-font-face :height gaelan/default-font-size)
  ;; Set the variable font face and height
  (set-face-attribute 'variable-pitch nil :font gaelan/default-variable-font-face :height gaelan/default-variable-font-size))

;; Starting emacs as a daemon confuses things because it doesn't necessarily know
;; it will be used in a GUI, which makes certain configuration calls misbehave since
;; they are run before an Emacs frame is launched.
;;
;; So here we set up fonts/icons immediately if we're not running as a daemon, and we
;; set up a special hook if we are running as a daemon.
(if (daemonp)
    (add-hook 'server-after-make-frame-hook
              (lambda ()
                (setq doom-modeline-icon t)
                (gaelan/set-font-faces)))
  (gaelan/set-font-faces))

(use-package all-the-icons
  :ensure t)

(use-package doom-modeline
  :ensure t
  :custom
  (doom-modeline-height 21)
  (doom-modeline-buffer-file-name-style 'truncate-upto-project)
  :config
  (doom-modeline-mode 1))

(use-package rebecca-theme
  ;; I guess themes can't be deferred
  :demand t
  :ensure t
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

(setq backup-directory-alist `(("." . "~/.emacs.d/backups"))
      delete-old-versions t
      kept-new-versions 8
      kept-old-versions 2
      version-control t)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq vc-follow-symlinks t)

(global-set-key (kbd "s-u") 'revert-buffer)

(global-set-key (kbd "s-o") 'other-window)

(global-auto-revert-mode +1)

(use-package which-key
  :ensure t
  :custom (which-key-idle-delay 1)
  :diminish which-key-mode
  :config
  (which-key-mode))

(use-package helpful
  :ensure t
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key)
  ("C-c C-d" . helpful-at-point))

(use-package async
  :ensure t
  :config
  ;; Because this command gets loaded from an unusal file path, we need
  ;; to explicitly set up the autoload
  (autoload 'dired-async-mode "dired-async.el" nil t)
  (dired-async-mode))

(setq fill-column 80)

(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize))

(setq-default mac-command-modifier 'meta)
(setq-default mac-option-modifier 'super)

(use-package pinentry
  :ensure t
  :custom
  (epa-pinentry-mode 'loopback)
  :config
  (pinentry-start))

(use-package helm
  :ensure t
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
  :ensure t
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
  :ensure t
  :after helm
  :config
  (helm-descbinds-mode))

(use-package projectile
  :ensure t
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map))
  :config
  (projectile-mode +1))

(use-package helm-projectile
  :ensure t
  :after helm
  :config
  (helm-projectile-on))

(use-package projectile-ripgrep
  :ensure t
  :after projectile)

(use-package helm-rg
  :ensure t
  :after helm)

(use-package perspective
  :ensure t
  :bind ("C-x C-b" . persp-list-buffers)
  :config (persp-mode))

(use-package persp-projectile
  :ensure t
  :after (perspective projectile)
  :bind (:map projectile-mode-map
              (("s-s" . projectile-persp-switch-project-project))))

(use-package treemacs
  :ensure t)

(use-package treemacs-projectile
  :ensure t
  :after projectile)

(use-package treemacs-magit
  :ensure t
  :after magit)

(use-package flycheck
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-flycheck-mode))

(use-package yasnippet-snippets
  :ensure t)
(use-package yasnippet
  :ensure t
  :after yasnippet-snippets
  :config
  (yas-global-mode 1))

(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :bind (("M-TAB" . 'company-complete)))

(use-package helm-company
  :ensure t
  :after (helm company)
  :config
  (define-key company-mode-map (kbd "C-:") 'helm-company)
  (define-key company-active-map (kbd "C-:") 'helm-company))

(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-more-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

(use-package direnv
  :ensure t
  :config
  (direnv-mode))

(use-package nix-sandbox
  :ensure t
  :after flycheck
  :config
  ; (setq flycheck-command-wrapper-function
  ;      (lambda (command) (apply 'nix-shell-command (nix-current-sandbox) command))
  ;      flycheck-executable-find
  ;      (lambda (cmd) (nix-executable-find (nix-current-sandbox) cmd))))
  )
(use-package helm-nixos-options
  :ensure t
  :after helm
  :if gaelan/*is-linux*
  :bind (("C-c C-S-n" . helm-nixos-options)))

(use-package company-nixos-options
  :ensure t
  :if gaelan/*is-linux*
  :after company
  :config (add-to-list 'company-backends 'company-nixos-options))

(use-package pdf-tools)

(use-package nov
  :ensure t
  :mode ("\\.epub\\'" . nov-mode))

(defconst gaelan/webdav-prefix
  (if gaelan/*is-osx*
      (file-name-as-directory "~/Seafile/DocStore/")
    (file-name-as-directory "~/fallcube/DocStore/"))
  "The root location of my emacs / org-mode files system")

(defconst gaelan/brain-prefix
  (concat gaelan/webdav-prefix "brain/")
  "The root directory of my org-roam knowledge store.")

(defconst gaelan/gtd-prefix
  (concat gaelan/webdav-prefix "gtd/")
  "The root directory of my GTD task management system.")

(defun gaelan/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode))

(use-package org
  :ensure t
  :hook
  (org-mode . gaelan/org-mode-setup)
  :custom
  ;; Have prettier chrome for headlines that can be expanded
  (org-ellipsis " ▾")
  ;; Show task state change logs in agenda mode
  (org-agenda-start-with-log-mode  t)
  ;; When we finish a task, log the time
  (org-log-done 'time)
  ;; Store task state changes into a dedicated drawer
  (org-log-into-drawer t)

  ;; The workhorse files in my GTD system
  (org-agenda-files
   `(,(concat gaelan/gtd-prefix "inbox.org")
     ,(concat gaelan/gtd-prefix "gtd.org")
     ,(concat gaelan/gtd-prefix "tickler.org")
     ,(concat gaelan/gtd-prefix "gcal/personal.org")
     ,(concat gaelan/gtd-prefix "gcal/work.org")))

  ;; Things I want to quickly enter, tasks and journal entries
  (org-capture-templates
   `(("t" "Todo" entry (file ,(concat gaelan/gtd-prefix "inbox.org"))
      "* TODO %?")
     ("d" "Daily Morning Reflection" entry (function gaelan/org-journal-find-location)
      "* %(format-time-string org-journal-time-format)Daily Morning Reflection\n** Things that will be achieved today\n     - [ ] %?\n** What am I grateful for?\n")
     ("e" "Daily Evening Reflection" entry (function gaelan/org-journal-find-location)
      "* %(format-time-string org-journal-time-format)Daily Evening Reflection\n** What were my wins today?\n   1. %?\n** What did I learn today?\n** What did not go according to plan today?\n** What did I do to help my future?\n** What did I do to help others?\n" :unnarrowed t)
     ("w" "Weekly Reflection" entry (function gaelan/org-journal-find-location)
      "* %(format-time-string org-journal-time-format)Weekly Reflection\n** What were you grateful for this week? Pick one and go deep.\n   %?\n** What were your biggest wins this week?\n** What tensions are you feeling this week? What is causing these tensions?\n** What can wait to happen this week? What can you work on this week?\n** What can you learn this week?" :unnarrowed t)
     ("m" "Monthly Reflection" entry (function gaelan/org-journal-find-location)
      "* %(format-time-string org-journal-time-format)Monthly Reflection\n** What were your biggest wins of the month?\n   - %?\n** What were you most grateful for this month?\n** What tensions have you removed this month?\n** What did you learn this month?\n** How have you grown this month?" :unnarrowed t)
     ("y" "Yearly Reflection" entry (function gaelan/org-journal-find-location)
      "* %(format-time-string) org-journal-time-format)Yearly Reflection\n** What were your biggest wins of the year?\n   - %?\n** What were you most grateful for this year?\n** What tensions have you removed this year?\n** What did you learn this year?\n** How have you grown this year?" :unnarrowed t)))

  ;; Where do I tend to move files to?
  (org-refile-targets
   `((,(concat gaelan/gtd-prefix "inbox.org") . (:level . 0))
     (,(concat gaelan/gtd-prefix "gtd.org") . (:maxlevel . 2))
     (,(concat gaelan/gtd-prefix "someday.org") . (:level . 1))
     (,(concat gaelan/gtd-prefix "tickler.org") . (:level . 1))
     ;; Move targets within a file
     (nil . (:level . 1))))

  ;; Handy search views for agenda mode
  (org-agenda-custom-commands
   '(("n" "Current Actions"
      ((todo "NEXT")
       (todo "STARTED")
       (todo "WAITING")))
     ("i" "Inbox Items"
      ((tags "+CATEGORY=\"Inbox\"")))
     ("u" "Unplanned Projects"
      ((tags-todo "PROJECT/PLAN")))
     ("p" "All Projects"
      ((tags-todo "PROJECT")))))
  (org-stuck-projects
   '("+PROJECT+LEVEL=2/-COMPLETED-ABANDONED-PAUSED" ("TODO" "NEXT" "STARTED") nil ""))

  :config
  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c c" . org-capture)))

(use-package visual-fill-column
  :ensure t
  :init
  (defun gaelan/org-mode-visual-fill ()
    (setq visual-fill-column-width 100
          visual-fill-column-center-text t)
    (visual-fill-column-mode 1))
  :after org
  :hook
  (org-mode . gaelan/org-mode-visual-fill))

(use-package org-habit
  ; This package is included with org, so don't ensure it.
  :ensure nil
  :after org
  :custom
  (org-habit-graph-column 60)
  :init
  (add-to-list 'org-modules 'habit))

(use-package org-roam
  :ensure t
  :hook (after-init . org-roam-setup)
  :custom
  (org-roam-directory gaelan/brain-prefix)
  (org-roam-completion-system 'helm)
  (org-roam-capture-templates
   '(("d" "default" plain "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
			 "#+title: ${title}\n")
      :unnarrowed t)
     ("f" "fleeting" plain "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
			 "#+title: ${title}\n#+FILETAGS: fleeting\n")
      :unnarrowed t)
     ("p" "people" plain "%?"
      :if-new (file+head "private/%<%Y%m%d%H%M%S>-${slug}.org.gpg"
			 "#+title: ${title}")
      :unnarrowed t)
     ("r" "reference" plain "%?"
      :if-new (file-head "references/${citekey.org"
			 "#+title: ${title}\n")
      :unnarrowed t)))
  :bind (("C-c n t" . org-roam-buffer-toggle)
	 ("C-c n b" . org-roam-buffer)
	 ("C-c n i" . org-roam-node-insert)
	 ("C-c n f" . org-roam-node-find)
	 ("C-c n g" . org-roam-graph))
  :init
  (setq org-roam-v2-ack t)
  (add-to-list 'display-buffer-alist
	       '("\\*org-roam\\*"
		 (display-buffer-in-direction)
		 (direction . right)
		 (window-width . 0.33)
		 (window-height . fit-window-to-buffer))))

(use-package deft
  :ensure t
  :after org
  :bind (:map org-mode-map
	      (("C-c n d" . deft)))
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory (concat gaelan/brain-prefix)))

(use-package helm-bibtex
  :ensure t
  :custom
  (bibtex-completion-bibliography (list (concat gaelan/brain-prefix "literature/REFERENCES.bib")))
  :after helm)

(use-package org-roam-bibtex
  :ensure t
  :after org-roam
  :bind (("C-c n a" . orb-note-actions)
         ("C-c n l" . orb-insert-link))
  :hook (after-init . org-roam-bibtex-mode)
  :custom
  (orb-note-actions-interface 'helm)
  (orb-autokey-format "%a%y%t")
  :config
  (require 'org-ref))

(use-package org-journal
  :ensure t
  :after org
  :bind ("C-c n j" . org-journal-new-entry)
  :custom
  (org-journal-date-format "%A, %F")
  (org-journal-file-type 'yearly)
  (org-journal-dir (file-name-as-directory (concat gaelan/webdav-prefix "journal")))
  (org-journal-file-format "%Y.org"))

(defun gaelan/org-journal-find-location ()
  ;; Open today's journal, but specify a non-nil prefix argument in order to
  ;; inhibit inserting the heading; org-capture will insert the heading.
  (org-journal-new-entry t)
  (unless (eq org-journal-file-type 'daily)
    (while (> (org-current-level) 1)
      (org-up-heading-safe))
    (org-narrow-to-subtree))
  (goto-char (point-min)))

(use-package org-pomodoro
  :ensure t
  :init
  (setq org-pomodoro-start-spound (concat user-emacs-directory "audio/ds9intercom.mp3")
        org-pomodoro-finished-sound (concat user-emacs-directory "audio/ds9intercom.mp3")
        org-pomodoro-overtime-sound (concat user-emacs-directory "audio/ds9intercom.mp3")
        org-pomodoro-short-break-sound (concat user-emacs-directory "audio/ds9intercom.mp3")
        org-pomodoro-long-break-sound (concat user-emacs-directory "audio/computerbeepsequence1.mp3")
        org-pomodoro-ticking-sound (concat user-emacs-directory "audio/incoming_hail2.mp3")))

(use-package org-noter
  :ensure t)

(use-package org-bullets
  :ensure t
  :after org
  :hook (org-mode . org-bullets-mode))

(use-package org-gcal
  :ensure t
  :init
  (load-library (concat user-emacs-directory "secrets.el"))
  (setq org-gcal-client-id gaelan/gcal-client-id)
  (setq org-gcal-client-secret gaelan/gcal-client-secret)
  (setq org-gcal-file-alist
        `(("gdcosta@gmail.com" . ,(concat gaelan/gtd-prefix "gcal/personal.org"))
          ("gaelan@tulip.com" . ,(concat gaelan/gtd-prefix "gcal/work.org")))))

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

;; Automatically tangle our Emacs.org config file when we save it
(defun gaelan/org-babel-tangle-config ()
  (when (string-equal (file-name-directory (buffer-file-name))
		      (expand-file-name user-emacs-directory))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'gaelan/org-babel-tangle-config)))

(use-package ox-hugo
  :ensure t
  :after ox)

(use-package vterm
  :ensure t)

(use-package magit
  :ensure t
  ;; I should have a keybinding that displays magit-status from anywhere
  :bind (("C-x g" . magit-status))
  :config
  ;; Enable pseudo-worktree for uncommitted files.
  (require 'magit-wip)
  (magit-wip-mode))

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  ;; Enable some built-in LSP clients
  :hook (go-mode . lsp-deferred))

(use-package lsp-ui
  :ensure t
  :after lsp-mode)

(use-package lsp-treemacs
  :ensure t
  :after lsp-mode
  :config
  (lsp-treemacs-sync-mode +1))

(use-package helm-lsp
  :ensure t)

(use-package dap-mode
  :ensure t
  :config (dap-auto-configure-mode))

(use-package docker
  :ensure t
  :bind ("C-c d" . docker))

(use-package docker-tramp
  :ensure t)

(use-package rainbow-delimiters
  :ensure t)

(show-paren-mode)

(use-package smartparens
  :ensure t
  :config
  (require 'smartparens-config)
  (show-smartparens-global-mode +1))

(defun gaelan/generic-lisp-mode-hook ()
  "Mode hook when working in any Lisp."
  ;; Unlike non-lispy editing modes, we should never allow unbalanced parens
  (smartparens-strict-mode)
  ;; Enable visual disambiguation of nested parentheses
  (rainbow-delimiters-mode)
  ;; Show documentation for a function/variable in the minibuffer
  (turn-on-eldoc-mode))

(use-package sly
  :ensure t)
(use-package sly-quicklisp
  :ensure t)

(use-package helm-sly
  :ensure t
  :after (sly helm-company)
  :config
  (add-hook 'sly-mrepl-hook #'company-mode)
  ; (define-key sly-mrepl-mode-map (kbd "<tab>") 'helm-company)
  )

(setq inferior-lisp-program "sbcl")

(add-hook 'lisp-mode-hook 'gaelan/generic-lisp-mode-hook)

(add-hook 'emacs-lisp-mode-hook 'gaelan/generic-lisp-mode-hook)

(define-key emacs-lisp-mode-map (kbd "C-c C-c") 'eval-defun)
(define-key emacs-lisp-mode-map (kbd "C-c C-p") 'eval-print-last-sexp)
(define-key emacs-lisp-mode-map (kbd "C-c C-r") 'eval-region)
(define-key emacs-lisp-mode-map (kbd "C-c C-k") 'eval-buffer)
(define-key emacs-lisp-mode-map (kbd "C-c C-l") 'load-file)
(define-key emacs-lisp-mode-map (kbd "C-c RET") 'macroexpand-1)
(define-key emacs-lisp-mode-map (kbd "C-c M-m") 'macroexpand-all)

(use-package clojure-mode
  :ensure t
  :config
  (add-hook 'clojure-mode-hook #'gaelan/generic-lisp-mode-hook)
  (add-hook 'clojure-mode-hook #'subword-mode))

(use-package cider
  :ensure t
  :config
  (add-hook 'cider-repl-mode-hook #'gaelan/generic-lisp-mode-hook)
  (add-hook 'cider-repl-mode-hook #'subword-mode))

(use-package helm-cider
  :ensure t
  :after helm)

(defun gaelan/clj-refactor-hook ()
  (clj-refactor-mode 1)
  (yas-minor-mode 1)
  (cljr-add-keybindings-with-prefix "C-c C-m"))

(use-package clj-refactor
  :ensure t
  :config
  (add-hook 'clojure-mode-hook #'gaelan/clj-refactor-hook))

(use-package flycheck-clj-kondo
  :ensure t
  :after clojure-mode)

(use-package cider-eval-sexp-fu
  :ensure t)

(use-package kaocha-runner
  :ensure t
  :bind ((:map clojure-mode-map
               ("C-c k t" . kaocha-runner-run-test-at-point)
               ("C-c k r" . kaocha-runner-run-tests)
               ("C-c k a" . kaocha-runner-run-all-tests)
               ("C-c k w" . kaocha-runner-show-warnings)
               ("C-c k h" . kaocha-runner-hide-windows))))

(use-package go-mode
  :ensure t)

(use-package pyenv-mode
  :ensure t
  :config
  (add-hook 'python-mode 'pyenv-mode))

(use-package anaconda-mode
  :ensure t
  :config
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode))

(use-package company-anaconda
  :ensure t
  :after company
  :config
  (add-to-list 'company-backends '(company-anaconda :with company-capf)))

(use-package lsp-haskell
  :ensure t
  :hook (haskell-mode-hook . lsp-deferred))

(use-package rustic
  :ensure t)

(use-package terraform-mode
  :ensure t)

(use-package company-terraform
  :ensure t
  :after company
  :config
  (company-terraform-init))

(use-package yaml-mode
  :ensure t)

(use-package nix-mode
  :ensure t)

(defun gaelan/run-in-background (command)
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

(defun gaelan/set-wallpaper ()
  (interactive)
  (start-process-shell-command
   "feh" nil "feh --bg-scale ~/Pictures/Wallpaper/Vapourwave.jpg"))

(defun gaelan/exwm-init-hook ()
  ;; Make workspace 1 be the one which we activate at startup
  (exwm-workspace-switch-create 1)

  ;; Start our dashboard panel
  ;; (gaelan/start-panel)

  ;; Launch apps that will run in the background
  (gaelan/run-in-background "dunst")
  (gaelan/run-in-background "nm-applet")
  (gaelan/run-in-background "pasystray")
  (gaelan/run-in-background "blueman-applet")
  (gaelan/run-in-background "seafile-applet"))

(defun gaelan/exwm-update-title-hook ()
  "EXWM hook for renaming buffer names to their associated X window title."

  (pcase exwm-class-name
    ("Firefox" (exwm-workspace-rename-buffer
                (format "Firefox: %s" exwm-title)))))

(defun gaelan/exwm-update-class-hook ()
  "EXWM hook for renaming buffer names to their associated X window class."
  (exwm-workspace-rename-buffer exwm-class-name))

(defun gaelan/exwm-randr-screen-change-hook ()
  (gaelan/run-in-background "autorandr --change --force")
  (gaelan/set-wallpaper)
  (message "Display config: %s"
           (string-trim (shell-command-to-string "autorandr --current"))))

(use-package exwm
  :demand t
  :if gaelan/*is-linux*
  :ensure t
  :bind
  (:map exwm-mode-map
        ;; C-q will enable the next key to be sent directly
        ([?\C-q] . 'exwm-input-send-next-key))
  :config
  ;; Set default number of workspaces
  (setq exwm-workspace-number 5)

  ;; Set up management hooks
  (add-hook 'exwm-update-class-hook
            #'gaelan/exwm-update-class-hook)
  (add-hook 'exwm-update-title-hook
            #'gaelan/exwm-update-title-hook)
  ;; (add-hook 'exwm-manage-finish-hook
  ;;  	      #'gaelan/exwm-manage-finish-hook)
  (add-hook 'exwm-init-hook
            #'gaelan/exwm-init-hook)

  ;; Enable multi-monitor support for EXWM
  (require 'exwm-randr)
  ;; Configure monitor change hooks
  (add-hook 'exwm-randr-screen-change-hook
            'gaelan/exwm-randr-screen-change-hook)
  (exwm-randr-enable)
  ;; Call the monitor configuration hook for the first time
  (gaelan/run-in-background "autorandr --change --force")
  (gaelan/set-wallpaper)

  ;; My workspaces includes specific ones for browsing, mail, slack
  ;; By default, workspaces show up on the first, default, active monitor.
  (setq exwm-randr-workspace-monitor-plist
        '(1 "DP-1-1" 2 "DP-1-1" 3 "DP-2" 4 "DP-2"))

  ;; Set up exwm's systembar since xmobar doesn't support it
  ;; Note: This has to be done before (exwm-init)
  (require 'exwm-systemtray)
  (setq exwm-systemtray-height 20)
  (exwm-systemtray-enable)

  ;; Automatically send mouse cursor to selected workspace's display
  (setq exwm-workspace-warp-cursor t)

  ;; Window focus should follow mouse pointer
  (setq mouse-autoselect-window t
        focus-follows-mouse t)

  ;; Set some global window management bindings. These always work
  ;; regardless of EXWM state.
  ;; Note: Changing this list after (exwm-enable) takes no effect.   
  (setq exwm-input-global-keys
        `(
          ;; 's-r': Reset to (line-mode).
          ([?\s-r] . exwm-reset)

          ;; Move between windows
          ([?\s-h] . windmove-left)
          ([?\s-l] . windmove-right)
          ([?\s-k] . windmove-up)
          ([?\s-j] . windmove-down)

          ;; 's-w': Switch workspace.
          ([?\s-w] . exwm-workspace-switch)
          ;; 's-b': Bring application to current workspace
          ([?\s-b] . exwm-workspace-switch-to-buffer)

          ;; s-0 is an inconvenient shortcut sequence, given 0 is before 1
          ([?\s-`] . (exwm-workspace-switch-create 0))
          ([s-escape] . (exwm-workspace-switch-create 0))

          ;; 's-p': Launch application a la dmenu
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

  ;; Certain important emacs keystrokes should always be handled by
  ;; emacs in preference over the application handling them
  ;; (setq exwm-input-prefix-keys
  ;; 	  '(?\C-x
  ;; 	    ?\C-u
  ;; 	    ?\C-h
  ;; 	    ?\M-x
  ;; 	    ?\M-`
  ;; 	    ?\M-&
  ;; 	    ?\M-:))

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
          ;; search (this should really be a firefox-only thing)
          ([?\C-s] . [?\C-f])))

  ;; Pin certain applications to specific workspaces
  (setq exwm-manage-configurations
        '(((string= exwm-class-name "Firefox") workspace 2)
          ((string= exwm-class-name "Chromium-browser") workspace 3)
          ((string= exwm-class-name ".obs-wrapped") workspace 2)))

  ;; Enable EXWM
  (exwm-enable))

(with-eval-after-load 'ediff-wind
  (setq ediff-control-frame-parameters
        (cons '(unsplittable . t)  ediff-control-frame-parameters)))

(use-package desktop-environment
  :ensure t
  :if gaelan/*is-linux*
  :after exwm
  :config
  (desktop-environment-mode))

(use-package helm-exwm
  :ensure t
  :if gaelan/*is-linux*
  :init
  (setq-default helm-source-names-using-follow '("EXWM buffers"))
  :config
  (setq helm-exwm-emacs-buffers-source (helm-exwm-build-emacs-buffers-source))
  (setq helm-exwm-source (helm-exwm-build-source))
  (push 'helm-exwm-emacs-buffers-source helm-mini-default-sources)
  (push 'helm-exwm-source helm-mini-default-sources))
