;;; init-misc.el -- Miscellaneous configurartion. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;; Handy shortcut for reverting buffers
(global-set-key (kbd "s-u") 'revert-buffer)

;; It is quicker to type y/n to prompts than "yes" or "no".
(defalias 'yes-or-no-p 'y-or-n-p)

;; Don't ask about following symlinks, just do it.
(setq vc-follow-symlinks t)

(provide 'init-misc)
;;; init-misc.el ends here
