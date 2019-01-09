;; Load the package manager
(require 'package)

;; Don't enable packages ... yet
(setq package-enable-at-startup nil)

;; Add package sources
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("marmalade" . "https://marmalade-repo.org/packages/"))

;; Initialize package manager
(package-initialize)

;; Don't ask about following symlinks, just do it.
(setq vc-follow-symlinks t)

;; Load config.org configuration
(org-babel-load-file (expand-file-name "~/.emacs.d/config.org"))
