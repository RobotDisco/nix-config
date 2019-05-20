;;; Helm

(when (require 'helm-descbinds nil t)
  (helm-descbinds-mode))

(when (require 'helm-ls-git nil t)
  ;; `helm-source-ls-git' must be defined manually
  ;; See https://github.com/emacs-helm/helm-ls-git/issues/34
  (setq helm-source-ls-git
	(and (memq 'helm-source-ls-git helm-ls-git-default-sources)
	     (helm-make-source "Git files" 'helm-ls-git-source
	       :fuzzy-match helm-ls-git-fuzzy-match))))

(helm-mode 1)
(add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)

(provide 'gaelan/init-helm)
