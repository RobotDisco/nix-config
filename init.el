(require 'cask "~/.cask/cask.el")
(cask-initialize)

(tool-bar-mode nil)

(global-auto-revert-mode t)
(show-paren-mode t)

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

;; Cider
(add-hook 'cider-mode-hook 'cider-turn=-on-eldoc-mode)
(add-hook 'cider-repl-mode-hook 'smartparens-strict-mode)
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)

;; Clojure auto-complete
(add-hook 'cider-repl-mode-hook 'ac-nrepl-setup)
(add-hook 'cider-mode-hook 'ac-nrepl-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'cider-repl-mode))

;; Clojure auto-complete for tab completion
(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))
(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'cider-repl-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'cider-mode-hook 'set-auto-complete-as-completion-at-point-function)

;; Use ac-nrepl insetead of nrepl-doc
(eval-after-load "cider"
  '(define-key cider-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc))


(global-set-key (kbd "C-x g") 'magit-status)

(add-hook 'before-save-hook 'whitespace-cleanup)

(setq inhibit-splash-screen t)
