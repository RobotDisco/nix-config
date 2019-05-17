;;; Store additional config in a 'lisp' subfolder and add it to the load path
;;; so that 'require' can find the files.
;;; This must be done before moving `user-emacs-directory'.
(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))

;; Load the package manager
(require 'package)

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; Who am I?
(setq user-full-name "Gaelan D'costa"
      user-mail-address "gdcosta@gmail.com")

;; Add package sources
(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
; (add-to-list 'package-archives
;	     '("marmalade" . "https://marmalade-repo.org/packages/"))

;; Remove unnecessary chrome.
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(fringe-mode 1)

;; It is quicker to type y/n to prompts than "yes" or "no".
(defalias 'yes-or-no-p 'y-or-n-p)

;; Don't ask about following symlinks, just do it.
(setq vc-follow-symlinks t)

;; Isolate custom variables because emacs likes to muck with these, making it hard to manage in source control.
(setq custom-file "~/.emacs.d/custom.el")

;; Install magit for managing git repos
(require 'magit)

;; All my org files live in a cloud-synced directory that differ between OSX and Linux
(require 'org)
(setq gaelan/webdav-prefix
      (if (eql system-type 'darwin)
	  (file-name-as-directory "~/Seafile/emacs/")
	(file-name-as-directory "~/fallcube/emacs/")))
(require 'org-journal)
(customize-save-variable 'org-journal-dir
			 (file-name-as-directory (concat gaelan/webdav-prefix "journal/")))
(customize-save-variable 'org-journal-file-format "%Y/%Y%m%d.org")
;; Bullet Journal discourages carrying over todos. Decide that explicitly!
(customize-save-variable 'org-journal-carryover-items nil)
(customize-save-variable 'org-agenda-file-regex "\`[^.].*\.org\'\|\`[0-9]+\'")
;; Prettify org mode, remove unnecessary asterix.
(require 'org-bullets)
(add-hook 'org-mode
	  (lambda ()
	    (org-bullets-mode 1)))

;; I use Emacs as my window manager
(nconc package-selected-packages '(exwm))
(eval-after-load 'exwm
  (require 'gaelan/init-exwm))
