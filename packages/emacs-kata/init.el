;;; init.el --- uConsole code-kata Emacs configuration -*- lexical-binding: t -*-

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Packages are provided by the Nix `emacs-kata` derivation, which parses
;; this file for `:ensure t` use-package forms. Make :ensure a no-op at
;; runtime so use-package never contacts a package archive.
(eval-when-compile (require 'use-package))
(eval-and-compile
  (defun gaelan/use-package-ensure-ignore (&rest _args) t)
  (setq use-package-ensure-function #'gaelan/use-package-ensure-ignore)
  (setq use-package-always-defer t)
  (setq use-package-hook-name-suffix nil))

;; Sentinel: confirms the Nix builder parses this .el for :ensure forms.
;; Replaced by the full config in Task 3.
(use-package vertico
  :ensure t
  :defer 1
  :config (vertico-mode +1))
