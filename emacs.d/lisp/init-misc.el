;;; init-misc.el -- Miscellaneous configurartion. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(require 'init-package)

;; Window systems like OSX should set path based on shell configuration.
(gaelan/require-package 'exec-path-from-shell)
(when (require 'exec-path-from-shell nil t)
  (when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize)))

;; Handy shortcut for reverting buffers
(global-set-key (kbd "s-u") 'revert-buffer)

;; Automatically revret files if they have been changed from under the editor
(global-auto-revert-mode +1)

;;; Syntax checking
(when (gaelan/require-package 'flycheck)
  (add-hook 'after-init-hook #'global-flycheck-mode))

;;; Which key mode
;;; If we start a key chord, this tells us what actions we can complete via that chord.
(when (gaelan/require-package 'which-key)
  (which-key-mode))

;; Clojure support
(gaelan/require-package 'cider)

;; Install magit for managing git repos
(when (gaelan/require-package 'magit)
  (global-set-key (kbd "C-x g") 'magit-status)
  (when (require 'magit-wip nil t)
    (magit-wip-mode)))

;;; Project Management
(when (gaelan/require-package 'projectile)
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))
(when (gaelan/require-package 'helm-projectile)
  (helm-projectile-on))
(gaelan/require-package 'projectile-ripgrep)

(provide 'init-misc)
;;; init-misc.el ends here
