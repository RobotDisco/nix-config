;; Configure package.el subsystem before initializing
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
	("melpa-stable" . "https://stable.melpa.org/packages/")
	("melpa" . "https://melpa.org/packages/")
	("marmalade" . "https://marmalade-repo.org/packages/")))
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

;; Use keychain to load SSH keys
(use-package keychain-environment
  :config (keychain-refresh-environment))

;; Add OSX path when run graphically
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config (exec-path-from-shell-initialize))

;; Use Anonymous Pro as default font
(add-to-list 'default-frame-alist
	     '(font . "Anonymous Pro-14"))

;; Solarized looks gross in console-mode
(use-package solarized-theme
  ;; :if window-system
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

;; helm
(use-package helm
  :config
  (require 'helm-config)
  (helm-mode 1)
  (helm-linum-relative-mode 1)
  ;; Set fuzzy matching everywhere
  (setq helm-mode-fuzzy-math t)
  (setq helm-completion-in-region-fuzzy-match t)
  :bind (("M-x" . helm-M-x)
	 ("C-x C-f" . helm-find-files)
	 ("M-y" . helm-show-kill-ring)
	 ("C-x C-b" . helm-mini)))
;; See git info, when application, in helm windows
(use-package helm-ls-git)
;; Search active key bindings
(use-package helm-descbinds
  :diminish helm-descbinds-mode
  :config (helm-descbinds-mode))
(use-package helm-tramp)

;; Bind imenu to a useful key
(global-set-key (kbd "M-i") 'imenu)

;; That cool thing Christian has that lets you move to adjacent rows based on
;; relative position to the current cursor.
(use-package linum-relative)

;; Jump around to text efficiently
(use-package ace-jump-mode
  :config
  :bind (("C-:" . avy-goto-char)
	 ("C-'" . avy-goto-char-2)
	 ("M-g f" . avy-goto-line)
	 ("M-g w" . avy-goto-word-1)
	 ("M-g e" . avy-goto-word-0)))


;; pcomplete for fancy (e)shell buffer completion
(use-package pcomplete-extension)

;; Show available key bindings
(use-package which-key
  :config
  (which-key-mode))

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
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'lisp-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'lisp-interaction-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'ielm-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook 'rainbow-delimiters-mode))

(use-package magit
  :bind ("C-x g" . magit-status))

;; Auto-completion
(use-package company
  :config (add-hook 'after-init-hook 'global-company-mode))

;; Syntax checking
(use-package flycheck
  :config (add-hook 'after-init-hook #'global-flycheck-mode))

;; Errors hover over point of error, not in separate buffer
(use-package flycheck-pos-tip
  :config
  (eval-after-load 'flycheck '(setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))

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
(use-package cider
  :pin melpa-stable
  :config
  (add-hook 'cider-repl-mode-hook #'subword-mode))
(use-package cider-eval-sexp-fu)
(use-package clj-refactor
  :config
  (defun clj-refactor-mode-hook ()
    (clj-refactor-mode 1)
    (yas-minor-mode 1)
    ;; This choice of keybinding leaves cider-macroexpand-1 unbound
    (cljr-add-keybindings-with-prefix "C-c C-m"))
  (add-hook 'clojure-mode-hook #'clj-refactor-mode-hook))
(use-package flycheck-clojure
  :config
  (eval-after-load 'flycheck '(flycheck-clojure-setup)))

(use-package smartparens
  :config
  (require 'smartparens-config)
  (show-smartparens-global-mode t)
  (add-hook 'prog-mode-hook 'turn-on-smartparens-mode)
					; (add-hook 'clojure-mode-hook 'turn-on-smartparens-strict-mode)
  (add-hook 'cider-repl-mode-hook 'turn-on-smartparens-strict-mode)
					; (add-hook 'scheme-mode-hook 'turn-on-smartparens-strict-mode)
					; (add-hook 'lisp-mode-hook 'turn-on-smartparens-strict-mode)
  (add-hook 'lisp-interaction-mode-hook 'turn-on-smartparens-strict-mode)
					; (add-hook 'emacs-lisp-mode-hook 'turn-on-smartparens-strict-mode)
  (add-hook 'ielm-mode-hook 'turn-on-smartparens-strict-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook 'turn-on-smartparens-strict-mode)
  (sp-use-smartparens-bindings))

;; Hippie Expand (the best expand?)
(global-set-key "\M- " 'hippie-expand)

;; Adjust text scale should operate globally
(defadvice text-scale-adjust (around all-buffers (arg) activate)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      ad-do-it)))

;; Save recent files list every 5 minutes, inhibit output
(require 'recentf)
(run-at-time (* 5 60) nil
	     (lambda ()
	       (let ((inhibit-message t))
		 (recentf-save-list))))


;; Delete trailing whitespace from lines/buffer before every save
(use-package whitespace
  :config
  (add-hook 'before-save-hook 'whitespace-cleanup))

;; Disable default chrome
(tool-bar-mode -1)
(setq inhibit-startup-message t)

;; Show a bunch of useful things in the status mode line
(line-number-mode t)
(column-number-mode t)

;; enable y/n prompts instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

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
(use-package js2-mode)

(use-package rjsx-mode
  :config (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode)))

(use-package json-mode
  :config (add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode)))

;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
	      (append flycheck-disabled-checkers
		      '(javascript-jshint)))

(use-package skewer-mode
  :config
  (add-hook 'js2-mode-hook 'skewer-mode)
  (add-hook 'css-mode-hook 'skewer-css-mode)
  (add-hook 'html-mode-hook 'skewer-html-mode))

;; Org mode (GTD and the like)

(let ((gaelan-webdav-prefix (if (eql system-type 'darwin)
				(file-name-as-directory "/Volumes/webdav/")
			      (file-name-as-directory "~/webdav/"))))
  ;; Org mode
  (use-package org
    :commands org-store-link org-agenda org-capture org-iswitchb
    :bind (("C-c l" . org-store-link)
	   ("C-c a" . org-agenda)
	   ("C-c c" . org-capture)
	   ("C-c b" . org-iswitchb))
    :config
    (global-set-key "\C-cl" 'org-store-link)
    (global-set-key "\C-ca" 'org-agenda)
    (global-set-key "\C-cc" 'org-capture)
    (global-set-key "\C-cb" 'org-iswitchb)
    (setq org-directory (file-name-as-directory (concat gaelan-webdav-prefix "gtd")))
    (setq org-mobile-directory (file-name-as-directory (concat org-directory "mobileorg")))
    (setq org-mobile-inbox-for-pull (concat org-directory "inbox.org"))
    (setq org-agenda-files (list (concat org-directory "gtd.org")
				 (concat org-directory "tickler.org")))
    (setq org-agenda-custom-commands
	  '(("n" "Next tasks" todo "NEXT|STARTED")))
    (setq org-capture-templates `(("i" "Inbox" entry
				   (file ,(concat org-directory "inbox.org"))
				   "* TODO %i%?")
				  ("p" "Personal Tickler" entry
				   (file+headline ,(concat org-directory "tickler.org") "Personal")
				   "* TODO %i%? \n %U")
				  ("w" "Work Tickler" entry
				   (file+headline ,(concat org-directory "tickler.org") "Work")
				   "* TODO %i%? \n %U")))
    (setq org-refile-targets `((,(concat org-directory "gtd.org") :maxlevel . 2)
			       (,(concat org-directory "someday.org") :level . 1)
			       (,(concat org-directory "tickler.org") :level . 1)))
    (setq org-todo-keywords '((sequence
			       "TODO(t)"
			       "NEXT(n)"
			       "STARTED(s)"
			       "WAITING(w)"
			       "|"
			       "DONE(d)"
			       "CANCELLED(c)")))
    (setq org-tag-alist '((:startgroup . nil)
			  ("@home" . ?h)
			  ("@officeto" . ?t)
			  ("@officekw" . ?k)
			  ("@phone" . ?p)
			  ("@lappy" . ?l)
			  ("@online" . ?i)
			  ("@email" . ?e)
			  ("@errand" . ?r)
			  (:endgroup . nil)))
    (setq org-highest-priority ?A)
    (setq org-default-priority ?C)
    (setq org-lowest-priority ?D)
    (setq org-enforce-todo-checkbox-dependencies t)
    (setq org-agenda-dim-blocked-tasks t))

  (use-package org-journal
    :config
    (setq org-journal-dir (file-name-as-directory (concat gaelan-webdav-prefix "journal"))))

  (use-package org-wiki
    :config
    (setq org-wiki-location-list
      (list
       (concat gaelan-webdav-prefix
	       "theknowledge/personal")    ;; First wiki (root directory) is the default.
       (concat gaelan-webdav-prefix
	       "theknowledge/tulip")))
    (setq org-wiki-location (car org-wiki-location-list))))

;; Mail-Utility for Emacs (mu4e)
;; We assume mu + offlineimap have been setup properly
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu/mu4e")
(require 'mu4e)
(setq mu4e-mu-binary (if (eql system-type 'darwin)
			 "/usr/local/bin/mu"
		       "/usr/bin/mu"))
;; Call offlineimap to update mail.
(setq mu4e-get-mail-command "offlineimap")
;; use mu4e for e-mail in emacs
(setq mail-user-agent 'mu4e-user-agent)
;; maildir
(setq mu4e-maildir "~/mail")
(setq mu4e-sent-messages-behavior 'delete)
(setq mu4e-contexts
 `( ,(make-mu4e-context
     :name "Personal"
     :match-func (lambda (msg) (when msg
       (string-prefix-p "/personal" (mu4e-message-field msg :maildir))))
     :vars '(
       (mu4e-trash-folder . "/personal/trash")
       (mu4e-refile-folder . "/personal/archive")
       ))
   ,(make-mu4e-context
     :name "Tulip"
     :match-func (lambda (msg) (when msg
       (string-prefix-p "/tulip" (mu4e-message-field msg :maildir))))
     :vars '(
       (mu4e-trash-folder . "/tulip/trash")
       (mu4e-refile-folder . "/tulip/archive")
       ))
   ))

(use-package mu4e-alert
  :after (mu4e)
  :config
  (setq mu4e-alert-interesting-mail-query
       (concat "flag:unread maildir:/personal/inbox "
	       "OR "
	       "flag:unread maildir:/tulip/inbox"))
  (mu4e-alert-enable-mode-line-display)

  ;; Call offlineimap to update mail.
  (setq mu4e-get-mail-command "offlineimap -o")

  ;; I have my "default" parameters from Gmail
(setq mu4e-sent-folder "/personal/sent"
      ;; mu4e-sent-messages-behavior 'delete ;; Unsure how this should be configured
      mu4e-drafts-folder "/personal/drafts"
      mu4e-trash-folder "/personal/trash"
      mu4e-refile-folder "/personal/archive"
      user-mail-address "gdcosta@gmail.com"
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)

;; Now I set a list of
(defvar my-mu4e-account-alist
  '(("Personal"
     (mu4e-sent-folder "/personal/sent")
     (mu4e-drafts-folder "/personal/drafts")
     (user-mail-address "gdcosta@gmail.com")
     (smtpmail-smtp-user "gaelan")
     (smtpmail-local-domain "gmail.com")
     (smtpmail-default-smtp-server "smtp.gmail.com")
     (smtpmail-smtp-server "smtp.gmail.com")
     (smtpmail-smtp-service 587))
    ;; Include any other accounts here ...
    ("Tulip"
     (mu4e-sent-folder "/tulip/sent")
     (mu4e-drafts-folder "/tulip/drafts")
     (user-mail-address "gaelan@tulip.com")
     (smtpmail-smtp-user "gaelan")
     (smtpmail-local-domain "tulip.com")
     (smtpmail-default-smtp-server "smtp.gmail.com")
     (smtpmail-smtp-server "smtp.gmail.com")
     (smtpmail-smtp-service 587))))

(defun my-mu4e-set-account ()
  "Set the account for composing a message.
   This function is taken from:
     https://www.djcbsoftware.nl/code/mu/mu4e/Multiple-accounts.html"
  (let* ((account
    (if mu4e-compose-parent-message
	(let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
    (string-match "/\\(.*?\\)/" maildir)
    (match-string 1 maildir))
      (completing-read (format "Compose with account: (%s) "
	     (mapconcat #'(lambda (var) (car var))
	    my-mu4e-account-alist "/"))
	   (mapcar #'(lambda (var) (car var)) my-mu4e-account-alist)
	   nil t nil nil (caar my-mu4e-account-alist))))
   (account-vars (cdr (assoc account my-mu4e-account-alist))))
    (if account-vars
  (mapc #'(lambda (var)
      (set (car var) (cadr var)))
	account-vars)
      (error "No email account found"))))
(add-hook 'mu4e-compose-pre-hook 'my-mu4e-set-account))

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

;; Make sure S-u reverts buffer
(global-set-key (kbd "s-u") #'(lambda ()
				(interactive)
				(revert-buffer t nil nil)))

;; Move customizations out of init.el
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
