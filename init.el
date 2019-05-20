;;; Store additional config in a 'lisp' subfolder and add it to the load path
;;; so that 'require' can find the files.
;;; This must be done before moving `user-emacs-directory'.
(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))

;; Load the package manager, set repos.
(when (require 'package nil t)
  (add-to-list 'package-archives
	       '("melpa-stable" . "https://stable.melpa.org/packages/"))
  (add-to-list 'package-archives
	       '("melpa" . "https://melpa.org/packages/"))
  (package-initialize))

;; Who am I?
(setq user-full-name "Gaelan D'costa"
      user-mail-address "gdcosta@gmail.com")

;; Add package sources

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

;; Helm
(nconc package-selected-packages '(helm helm-descbinds helm-ls-git))
(when (require 'helm-config nil t) (require 'gaelan/init-helm))

;; Generic programming language tools
(nconc package-selected-packages '(rainbow-delimiters smartparens))

;; Emacs Lisp
;; Can't use `with-eval-after-load' here because `emacs-lisp-mode' is builtin
;; and thus is loaded before this config file runs
(require 'gaelan/init-emacs-lisp)

;; Install magit for managing git repos
(require 'magit)

;;; Org-mode
(nconc package-selected-packages '(org org-bullets))
(with-eval-after-load 'org (require 'gaelan/init-org))

;; I use Emacs as my window manager
(nconc package-selected-packages '(exwm))
(with-eval-after-load 'exwm
  (require 'gaelan/init-exwm))
