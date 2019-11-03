;;; init.el --- Starting configuration file -*- lexical-binding: t -*-

;;; Commentary:
;;; This is the file from which my configuration framework begins.
;;; Set bootstraps the framework (sets things that can't be deferred to other
;;; files) and then loads all of those files in tern.

;;; Code:
(let ((minver "26.1"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))

;; Store additional config in a 'lisp' subfolder and add it to the load path
;; so that 'require' can find the files.
;; This must be done before moving `user-emacs-directory'.
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(defconst *is-osx* (eq system-type 'darwin)
  "Is this operating system OSX?")
(defconst *is-linux* (eq system-type 'gnu/linux)
  "Is this operating system Linux?")

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

;; Initialize custom or non-packaged lisp code.
(require 'init-site-lisp)
;; Initialize package manager
(require 'init-package)

;; Who am I?
(setq user-full-name "Gaelan D'costa"
      user-mail-address "gdcosta@gmail.com")

;; We'll need to load common-lisp functionality, but since we're using macros
;; we can restrict it to the compliation step to minimize runtime bloat.
(eval-when-compile
  (require 'cl-lib))

;; Customize basic Emacs appearance
(require 'init-appearance)

(when *is-osx*
    (require 'init-osx))

;; Helm
(require 'init-helm)
;; Completion
(require 'init-company)
;; Emacs Lisp
;; Can't use `with-eval-after-load' here because `emacs-lisp-mode' is builtin
;; and thus is loaded before this config file runs
(require 'init-emacs-lisp)
;;; Bitwarden passage manager
(require 'init-bitwarden)
;;; Org-mode
(require 'init-org)
;;; Python
(require 'init-python)
;; Window Manager
(require 'init-exwm)
;; Add miscellaneous config that isn't large enough for its own module.
(require 'init-misc)

(provide 'init)
;;; init.el ends here
