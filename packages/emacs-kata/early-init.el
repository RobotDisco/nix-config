;;; early-init.el --- uConsole kata early init -*- lexical-binding: t -*-
(setq inhibit-startup-screen t
      use-dialog-box nil)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
;; Nix supplies all packages; configure no archives.
(setq package-archives nil)
