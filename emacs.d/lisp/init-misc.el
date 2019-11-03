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

;;; Syntax checking
(with-eval-after-load 'flycheck
  (add-hook 'after-init-hook #'global-flycheck-mode))

;;; Which key mode
;;; If we start a key chord, this tells us what actions we can complete via that chord.
(gaelan/require-package 'which-key)
(with-eval-after-load 'which-key
  (which-key-mode))

;; Clojure support
(gaelan/require-package 'cider)

;; Install magit for managing git repos
(gaelan/require-package 'magit)
(with-eval-after-load 'magit
  (global-set-key (kbd "C-x g") 'magit-status)
  (magit-wip-mode))

;;; Project Management
(gaelan/require-packages '(projectile helm-projectile projectile-ripgrep))
(with-eval-after-load 'projectile
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))
(with-eval-after-load 'helm-projectile
  (helm-projectile-on))

(provide 'init-misc)
;;; init-misc.el ends here
