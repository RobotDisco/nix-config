;;;; -*- mode: Lisp;-*-

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-safe-themes (quote ("fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default)))
 '(inhibit-startup-screen t)
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(load (expand-file-name "~/.quicklisp/slime-helper.el"))
;; Replace "sbcl" with the path to your implementation
(setq inferior-lisp-program "ccl64")

;; Add MELPA repo to ELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;; Why say 'yes' or 'no'
(defalias 'yes-or-no-p 'y-or-n-p)

;; Add area for custom elisp files
(add-to-list 'load-path "~/.emacs.d/elisp/")

;; Use a Solarized theme.
(load-theme 'solarized-dark t)

;; Use anonymous pro as my font
(set-face-attribute 'default nil :family "Anonymous Pro")

(require 'fill-column-indicator)
(add-hook 'after-change-major-mode-hook 'fci-mode)
(setq-default fill-column 80)

;; Automatically enable paredit when lisping.
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

;; Add support for Twee/Twine files.
(autoload 'twee-mode "twee.el" "Mode for editing Twee files." t)
(add-to-list 'auto-mode-alist '("\\.tw\\'" . twee-mode))
(add-hook 'twee-mode-hook 'turn-on-font-lock)

;; Interactively Do things
(require 'ido)
(ido-mode t)
(ido-everywhere t)
;; Make ido use flx
(require 'flx-ido)
(flx-ido-mode t)
;; disable ido faces to see flx highlights.
(setq ido-use-faces nil)

;; Use RVM's default ruby by default
(require 'rvm)
(rvm-use-default)

;; Enable flymake-ruby mode
;;(require 'flymake-ruby)
;;(add-hook 'ruby-mode-hook 'flymake-ruby-load)

;; Enable company mode globally
(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay 0.1)
(setq company-minimum-prefix-length 2)

;; Activate runtime ruby syntax completion.
(add-hook 'inf-ruby-mode-hook (lambda () (require 'inf-ruby-company)))

;; fly-make ruby
(require 'flymake-ruby)		
(add-hook 'ruby-mode-hook 'flymake-ruby-load)  (require 'flymake-ruby)
(add-hook 'ruby-mode-hook 'flymake-ruby-load)

;; AucTeX customizations
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
(setq TeX-PDF-mode t)

;; Enable projectile mode globally
(projectile-global-mode)

;; Set up Emacs-w3m
(setq browse-url-browser-function 'w3m-browse-url)
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
;; optional keyboard short-cut
(global-set-key "\C-xm" 'browse-url-at-point)


(defun kill-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))
