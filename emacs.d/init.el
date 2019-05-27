;;; init.el --- Starting configuration file -*- lexical-binding: t -*-

;;; Commentary:
;;; This is the file from which my configuration framework begins.
;;; Set bootstraps the framework (sets things that can't be deferred to other
;;; files) and then loads all of thos files in tern.

;;; Code:

;; Produce backtraces when errors occur.
(setq debug-on-error t)

(let ((minver "26.1"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))

;; Store additional config in a 'lisp' subfolder and add it to the load path
;; so that 'require' can find the files.
;; This must be done before moving `user-emacs-directory'.
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(defconst *is-osx* (eq system-type 'darwin))

;;----------------------------------------------------------------------------
;; Adjust garbage collection thresholds during startup, and thereafter
;;----------------------------------------------------------------------------
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;;----------------------------------------------------------------------------
;; Isolate custom variables because emacs likes to muck with these, making it
;; hard to manage in source control.
;;----------------------------------------------------------------------------
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(require 'init-site-lisp)
;; Load the package manager, set repos.
(when (require 'package nil t)
  (add-to-list 'package-archives
	       '("melpa-stable" . "https://stable.melpa.org/packages/"))
  (add-to-list 'package-archives
	       '("melpa" . "https://melpa.org/packages/"))
  (add-to-list 'package-archives
	       '("org" . "https://orgmode.org/elpa/") t)
  (package-initialize))

;;;

;; Who am I?
(setq user-full-name "Gaelan D'costa"
      user-mail-address "gdcosta@gmail.com")

;; Remove unnecessary chrome.
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(fringe-mode 1)

(eval-when-compile
  (require 'cl-lib))

;; Prettify necessary chrome
(nconc package-selected-packages '(rebecca-theme))
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
  (load-theme 'rebecca t))

(add-to-list 'default-frame-alist '((font . "Anonymous Pro-14")))

;; It is quicker to type y/n to prompts than "yes" or "no".
(defalias 'yes-or-no-p 'y-or-n-p)

;; Don't ask about following symlinks, just do it.
(setq vc-follow-symlinks t)

;; Window systems like OSX should set path based on shell configuration.
(nconc package-selected-packages '(exec-path-from-shell))
(when (require 'exec-path-from-shell nil t)
  (when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize)))

;; Helm
(nconc package-selected-packages '(helm helm-descbinds helm-ls-git))
(when (require 'helm-config nil t) (require 'gaelan/init-helm))

;; Generic programming language tools
(nconc package-selected-packages '(rainbow-delimiters smartparens))

;; Completion
(nconc package-selected-packages '(company helm-company))
(when (require 'company nil t) (require 'gaelan/init-company))

;; Emacs Lisp
;; Can't use `with-eval-after-load' here because `emacs-lisp-mode' is builtin
;; and thus is loaded before this config file runs
(require 'gaelan/init-emacs-lisp)

;; Install magit for managing git repos
(require 'magit)

;;; Org-mode
(nconc package-selected-packages '(org org-bullets))
(nconc package-selected-packages '(org org-bullets org-gcal))
(when (require 'org nil t) (require 'gaelan/init-org))

;;; Project Management
(nconc package-selected-packages '(projectile helm-projectile projectile-ripgrep))
(when (require 'projectile nil t)
  (when (require 'helm-config nil t)
    (require 'helm-projectile))
  (require 'projectile-ripgrep)
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;;; Syntax checking
(nconc package-selected-packages '(flycheck))
(when (require 'flycheck nil t) (require 'gaelan/init-flycheck))

;;; Which key mode
;;; If we start a key chord, this tells us what actions we can complete via that chord.
(nconc package-selected-packages '(which-key))
(when (require 'which-key nil t)
  (which-key-mode))

;; Window Manager
(nconc package-selected-packages '(exwm helm-exwm))
(with-eval-after-load 'exwm
  (require 'gaelan/init-exwm))

(provide 'init)
;; init.el ends here.
;;
