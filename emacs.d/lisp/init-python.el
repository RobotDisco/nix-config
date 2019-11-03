;;; init-python.el -- Python dev env configuration. -* lexical-binding: t -*-

;;; Commentary:

;;; Code:
(require 'init-package)

;; Use and manage python virtualenvs 
(gaelan/require-package pyenv-mode)
;; Get runtime python inspection via a little server
(gaelan/require-package anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)

;; Use company-mode's completion framework with anaconda as a backend
(gaelan/require-package company-anaconda)
(with-eval-after-load 'company
  (add-to-list 'company-backends '(company-anaconda :with company-capf)))

(provide 'init-python)
;;; init-python.el ends hee
