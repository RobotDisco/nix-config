;;; early-init.el --- uConsole kata early init -*- lexical-binding: t -*-

;; Maximize GC threshold during startup; restore after init completes.
(let ((normal-gc-cons-threshold gc-cons-threshold)
      (normal-gc-cons-percentage gc-cons-percentage))
  (setq gc-cons-threshold most-positive-fixnum
        gc-cons-percentage 1.0)
  (add-hook 'emacs-startup-hook
            (lambda ()
              (setq gc-cons-threshold normal-gc-cons-threshold
                    gc-cons-percentage normal-gc-cons-percentage))))

;; Disable file-name handlers during startup; restore after.
(let ((normal-file-name-handler-alist file-name-handler-alist))
  (setq file-name-handler-alist nil)
  (add-hook 'emacs-startup-hook
            (lambda ()
              (setq file-name-handler-alist
                    normal-file-name-handler-alist))))

(setq inhibit-compacting-font-caches t)
(setq frame-inhibit-implied-resize t)

(setq inhibit-startup-screen t
      use-dialog-box nil)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %0.03f seconds with %d garbage collections"
                     (float-time (time-subtract after-init-time
                                                before-init-time))
                     gcs-done)))

;; Nix supplies all packages; configure no archives.
(setq package-archives nil)
