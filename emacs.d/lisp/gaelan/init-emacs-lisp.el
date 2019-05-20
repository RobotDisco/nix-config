;;; Emacs Lisp

(require 'gaelan/init-smartparens)
(require 'rainbow-delimiters)

(defun gaelan/emacs-lisp-mode-hook ()
  (smartparens-strict-mode)
  (rainbow-delimiters-mode)
  (turn-on-eldoc-mode))

(add-hook 'emacs-lisp-mode-hook #'gaelan/emacs-lisp-mode-hook)
(add-hook 'ielm-mode-hook #'gaelan/emacs-lisp-mode-hook)

(provide 'gaelan/init-emacs-lisp)
