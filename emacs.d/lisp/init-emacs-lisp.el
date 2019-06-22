;;; init-emacs-lisp.el --- Emacs Lisp environment config

;;; Commentary:

;;; Code:
(require 'init-smartparens)
(gaelan/require-package 'rainbow-delimiters)

(defun gaelan/emacs-lisp-mode-hook ()
  "Mode hook when working in Emacs Lisp."
  (smartparens-strict-mode)
  (rainbow-delimiters-mode)
  (turn-on-eldoc-mode))

(add-hook 'emacs-lisp-mode-hook #'gaelan/emacs-lisp-mode-hook)
(add-hook 'ielm-mode-hook #'gaelan/emacs-lisp-mode-hook)

(define-key emacs-lisp-mode-map (kbd "C-c C-c") 'eval-defun)
(define-key emacs-lisp-mode-map (kbd "C-c C-p") 'eval-print-last-sexp)
(define-key emacs-lisp-mode-map (kbd "C-c C-r") 'eval-region)
(define-key emacs-lisp-mode-map (kbd "C-c C-k") 'eval-buffer)
(define-key emacs-lisp-mode-map (kbd "C-c C-l") 'load-file)
(define-key emacs-lisp-mode-map (kbd "C-c RET") 'macroexpand-1)
(define-key emacs-lisp-mode-map (kbd "C-c M-m") 'macroexpand-all)


(provide 'init-emacs-lisp)
;;; init-emacs-lisp ends here
