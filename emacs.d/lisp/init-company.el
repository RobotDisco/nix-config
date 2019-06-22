;;; init-company.el --- company mode configuration

;;; Commentary:
;;; Company mode autocompletion

;;; Code:
(require 'init-package)

(gaelan/require-package 'company)
(add-hook 'after-init-hook 'global-company-mode)
(global-set-key (kbd "M-TAB") 'company-complete)

(gaelan/require-package 'helm-company)
(with-eval-after-load 'company
  '(progn
     (define-key company-mode-map (kbd "C-:") 'helm-company)
     (define-key company-active-map (kbd "C-:") 'helm-company)))

(provide 'init-company)
;;; init-company.el ends here
