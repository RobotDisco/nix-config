;;; init-haskell.el --- Haskell Programming language support

;;; Commentary:
;; We are centreing around the use of the "haskell IDE Engine" haskell tooling, which
;; leverages the "Language Server Protocol"

;;; Code:
(require 'init-package)

(gaelan/require-package 'lsp-mode)
(add-hook 'haskell-mode-hook #'lsp-deferred)

(gaelan/require-package 'lsp-ui)
(add-hook 'lsp-mode-hook #'lsp-ui-mode)

(gaelan/require-package 'company-lsp)
(with-eval-after-load 'company
  (push 'company-lsp company-backends))

(gaelan/require-package 'lsp-treemacs)
(lsp-treemacs-sync-mode 1)

(gaelan/require-package 'helm-lsp)

(gaelan/require-package 'lsp-haskell)
(with-eval-after-load 'lsp-haskell
  (customize-set-variable lsp-haskell-process-path-hie "hie-wrapper"))

(provide 'init-haskell)
;;; init-haskell ends here
