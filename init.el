;;; init.el --- Starting configuration file -*- lexical-binding: t -*-

;;; Commentary:
;;; This is the file from which my configuration framework begins.
;;; Set bootstraps the framework (sets things that can't be deferred to other
;;; files) and then loads all of those files in tern.

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
(defconst *is-osx* (eq system-type 'darwin)
  "Is this operating system OSX?")

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
;; Initialize package manager
(require 'init-package)

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
(gaelan/require-package 'rebecca-theme)
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

(add-to-list 'default-frame-alist '(font . "Anonymous Pro-14"))

(when *is-osx*
    (setq mac-command-modifier 'meta)
    (setq mac-option-modifier 'super))

;; Handy shortcut for reverting buffers
(global-set-key (kbd "s-u") 'revert-buffer)

;; It is quicker to type y/n to prompts than "yes" or "no".
(defalias 'yes-or-no-p 'y-or-n-p)

;; Don't ask about following symlinks, just do it.
(setq vc-follow-symlinks t)

;; Window systems like OSX should set path based on shell configuration.
(gaelan/require-package 'exec-path-from-shell)
(when (require 'exec-path-from-shell nil t)
  (when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize)))

;; Helm
(require 'init-helm)

;; Generic programming language tools

;; Completion
(require 'init-company)

;; Emacs Lisp
;; Can't use `with-eval-after-load' here because `emacs-lisp-mode' is builtin
;; and thus is loaded before this config file runs
(require 'init-emacs-lisp)

;; Clojure support
(gaelan/require-package 'cider)

;; Install magit for managing git repos
(gaelan/require-package 'magit)
(with-eval-after-load 'magit
  (global-set-key (kbd "C-x g") 'magit-status)
  (magit-wip-mode))

;;; Bitwarden passage manager
(require 'init-bitwarden)

;;; Org-mode
(require 'init-org)

;;; Project Management
(gaelan/require-packages '(projectile helm-projectile projectile-ripgrep))
(with-eval-after-load 'helm-projectile
		      (require 'helm-projectile)
		      (projectile-mode +1)
		      (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;;; Syntax checking
(with-eval-after-load 'flycheck
  (add-hook 'after-init-hook #'global-flycheck-mode))

;;; Which key mode
;;; If we start a key chord, this tells us what actions we can complete via that chord.
(gaelan/require-package 'which-key)
(with-eval-after-load 'which-key
  (which-key-mode))

;; Window Manager
(require 'init-exwm)

(provide 'init)
;; init.el ends here
