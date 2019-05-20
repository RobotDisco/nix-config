;;;; Company Mode

(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(customize-set-variable 'company-idle-delay nil)
(customize-set-variable 'company-minimum-prefix-length nil)
(global-set-key (kbd "C-.") 'company-complete)

(require 'helm-company)
(define-key company-mode-map (kbd "C-:") 'helm-company)
(define-key company-active-map (kbd "C-:") 'helm-company)

(provide 'gaelan/init-company)
