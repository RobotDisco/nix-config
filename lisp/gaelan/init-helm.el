;;; init-helm.el -- Configuration for Helm

;;; Commentary:

;;; Code:
(require 'init-package)

(gaelan/require-package 'helm)
(gaelan/require-package 'helm-descbinds)
(gaelan/require-package 'helm-ls-git)
;; Shows me the keybindings that are currently available
;; enhances C-h b
(when (require 'helm-descbinds nil t)
  (helm-descbinds-mode))

;; A cool interface for looking at files in git projects
;; `helm-browse-project'
(when (require 'helm-ls-git nil t)
  ;; `helm-source-ls-git' must be defined manually
  ;; See https://github.com/emacs-helm/helm-ls-git/issues/34
  (setq helm-source-ls-git
	(and (memq 'helm-source-ls-git helm-ls-git-default-sources)
	     (helm-make-source "Git files" 'helm-ls-git-source
	       :fuzzy-match helm-ls-git-fuzzy-match))))

(helm-mode 1)
(add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)

;; Add recommended keybindings as found in Thierry Volpiatto's guide
;; http://tuhdo.github.io/helm-intro.html
(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(global-set-key (kbd "C-x r b" ) #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-b") #'helm-mini)
;; (global-set-key (kbd "M-y") #'helm-show-kill-ring)
(global-set-key (kbd "M-i") #'helm-semantic-or-imenu)
(global-set-key (kbd "M-s o") #'helm-occur)
(global-set-key (kbd "C-h SPC") #'helm-all-mark-rings)
(global-set-key (kbd "C-x c h r") #'helm-register)
(global-set-key (kbd "C-x c h g") #'helm-google-suggest)
(global-set-key (kbd "C-c h M-:") #'helm-eval-expression-with-eldoc)

;; Turn on fuzzy matching in a bunch of places
;; turn it off if it is irritating or slows down searches.
(setq-default helm-M-x-fuzzy-match t
	      helm-buffers-fuzzy-matching t
	      helm-recentf-fuzzy-match t
	      helm-semantic-fuzzy-match t
	      helm-imenu-fuzzy-match t
	      helm-locate-fuzzy-match t
	      helm-apropos-fuzzy-match t
	      helm-lisp-fuzzy-completion t)

;; Add helmized history searching functionality for a variety of interfaces,
;; `eshell', `shell-mode', `minibuffer', using the same C-c C-l binding.

(add-hook 'eshell-mode-hook
	  #'(lambda ()
	      (define-key 'eshell-mode-map (kbd "C-c C-l") #'helm-eshell-history)))

(add-hook 'shell-mode-hook
	  #'(lambda ()
	      (define-key 'shell-mode-map (kbd "C-c C-l") #'helm-comint-input-ring)))

(define-key minibuffer-local-map (kbd "C-c C-l") #'helm-minibuffer-history)

;;; This makes the copy and rename operations asynchronous.
(dired-async-mode)

(when (require 'helm-exwm nil t)
  (setq-default helm-source-names-using-follow '("EXWM buffers"))
  (setq helm-exwm-emacs-buffers-source (helm-exwm-build-emacs-buffers-source))
  (setq helm-exwm-source (helm-exwm-build-source))
  (setq-default helm-mini-default-sources `(helm-exwm-emacs-buffers-source
					    helm-exwm-source
					    helm-source-recentf
					    ,(when (boundp 'helm-source-ls-git)
					       'helm-source-ls-git)
					    helm-source-bookmarks
					    helm-source-bookmark-set
					    helm-source-buffer-not-found)))

(provide 'gaelan/init-helm)
;;; init-helm.el ends here
