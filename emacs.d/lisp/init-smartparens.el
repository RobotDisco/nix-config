;;; init-smartparens.el --- Config for parens editing mode.

;;; Commentary:

;;; Code:
(require 'init-package)

(gaelan/require-package 'smartparens)
(require 'smartparens-config)
(with-eval-after-load 'smartparens-config
  (sp-use-smartparens-bindings))

(provide 'init-smartparens)
;;; init-smartparens ends here
