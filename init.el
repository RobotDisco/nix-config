(require 'cask "~/.cask/cask.el")
(cask-initialize)

(menu-bar-mode nil)

(defalias 'yes-or-no-p 'y-or-n-p)

(when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize))
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)

(load-theme 'cyberpunk t)

(projectile-global-mode)

(ido-mode t)
(ido-everywhere t)
(flx-ido-mode t)

(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

(add-to-list 'auto-mode-alist '("Cask\\'" . emacs-lisp-mode))
(add-hook 'emacs-lisp-mode-hook (lambda ()
				  (smartparens-strict-mode t)
				  (rainbow-delimiters-mode t)
				  (turn-on-eldoc-mode)
				  (rainbow-mode)
				  (setq mode-name "EL")))
(add-hook 'ielm-mode-hook (lambda()
			    (smartparens-strict-mode t)
			    (rainbow-delimiters-mode t)
			    (whitespace-mode nil)
			    (turn-on-eldoc-mode)))

(require 'smartparens-config)

(add-hook 'prog-mode-hook (lambda ()
			    (smartparens-mode t)))
(add-hook 'prog-mode-hook 'flycheck-mode)

(add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))

(add-hook 'python-mode-hook 'jedi:setup)

(global-set-key (kbd "C-x g") 'magit-status)

(setq inhibit-splash-screen t)
