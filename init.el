(require 'cask)
(cask-initialize)

(tool-bar-mode nil)

(global-auto-revert-mode t)
(show-smartparens-global-mode t)

(guru-global-mode t)

(defalias 'yes-or-no-p 'y-or-n-p)

(when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize))
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)

;; Stop the popup dialogs boxes which hang Emacs on OS X
(when (memq window-system '(mac ns))
  (defadvice yes-or-no-p (around prevent-dialog activate)
    "Prevent yes-or-no-p from activating a dialog"
    (let ((use-dialog-box nil))
      ad-do-it))
  (defadvice y-or-n-p (around prevent-dialog-yorn activate)
    "Prevent y-or-n-p from activating a dialog"
    (let ((use-dialog-box nil))
      ad-do-it)))

(load-theme 'cyberpunk t)

(which-function-mode t)

;; Show the current function name in the header line
(which-function-mode)
(setq-default header-line-format
	      '((which-func-mode ("" which-func-format " "))))
(setq mode-line-misc-info
	    ;; We remove Which Function Mode from the mode line, because it's mostly
	    ;; invisible here anyway.
	    (assq-delete-all 'which-func-mode mode-line-misc-info))

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

(require 'virtualenvwrapper)
(venv-initialize-interactive-shells)
(venv-initialize-eshell)
(setq venv-location "~/.virtualenvs/")

(require 'auto-complete-config)
(ac-config-default)

;; Cider
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
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
(global-set-key (kbd "M-/") 'hippie-expand)

;; Smartparens key bindings

;;;;;;;;;;;;;;;;;;;;;;;;
;; keybinding management

(define-key sp-keymap (kbd "C-M-f") 'sp-forward-sexp)
(define-key sp-keymap (kbd "C-M-b") 'sp-backward-sexp)
(define-key sp-keymap (kbd "C-M-d") 'sp-down-sexp)
(define-key sp-keymap (kbd "C-M-a") 'sp-backward-down-sexp)
(define-key sp-keymap (kbd "C-M-e") 'sp-up-sexp)
(define-key sp-keymap (kbd "C-M-u") 'sp-backward-up-sexp)
(define-key sp-keymap (kbd "C-M-n") 'sp-next-sexp)
(define-key sp-keymap (kbd "C-M-p") 'sp-previous-sexp)
(define-key sp-keymap (kbd "C-S-a") 'sp-beginning-of-sexp)
(define-key sp-keymap (kbd "C-S-d") 'sp-end-of-sexp)

(define-key sp-keymap (kbd "C-M-k") 'sp-kill-sexp)
(define-key sp-keymap (kbd "C-- C-M-k") 'sp-backward-kill-sexp)

(define-key sp-keymap (kbd "C-M-w") 'sp-copy-sexp)
(define-key sp-keymap (kbd "C-- C-M-w") 'sp-backward-copy-sexp)

(define-key sp-keymap (kbd "M-<delete>") 'sp-unwrap-sexp)
(define-key sp-keymap (kbd "M-<backspace>") 'sp-backward-unwrap-sexp)

(define-key sp-keymap (kbd "C-M-t") 'sp-transpose-sexp)

(define-key sp-keymap (kbd "M-D") 'sp-splice-sexp)
(define-key sp-keymap (kbd "C-M-<delete>") 'sp-splice-sexp-killing-forward)
(define-key sp-keymap (kbd "C-M-<backspace>") 'sp-splice-sexp-killing-backward)
(define-key sp-keymap (kbd "C-S-<backspace>") 'sp-splice-sexp-killing-around)

(define-key sp-keymap (kbd "C-<right>") 'sp-forward-slurp-sexp)
(define-key sp-keymap (kbd "C-<left>") 'sp-forward-barf-sexp)
(define-key sp-keymap (kbd "C-M-<left>") 'sp-backward-slurp-sexp)
(define-key sp-keymap (kbd "C-M-<right>") 'sp-backward-barf-sexp)

(define-key sp-keymap (kbd "C-M-]") 'sp-select-next-thing)
(define-key sp-keymap (kbd "C-[") 'sp-select-previous-thing)

(define-key sp-keymap (kbd "C-]") 'sp-select-next-thing-exchange)
(define-key sp-keymap (kbd "C-- C-]" 'sp-select-previous-thing-exchange))

(define-key sp-keymap (kbd "M-F") 'sp-forward-symbol)
(define-key sp-keymap (kbd "M-B") 'sp-backward-symbol)

(define-key sp-keymap (kbd "H-t") 'sp-prefix-tag-object)
(define-key sp-keymap (kbd "H-p") 'sp-prefix-pair-object)
(define-key sp-keymap (kbd "H-s c") 'sp-convolute-sexp)
(define-key sp-keymap (kbd "H-s a") 'sp-absorb-sexp)
(define-key sp-keymap (kbd "H-s e") 'sp-emit-sexp)
(define-key sp-keymap (kbd "H-s p") 'sp-add-to-previous-sexp)
(define-key sp-keymap (kbd "H-s n") 'sp-add-to-next-sexp)
(define-key sp-keymap (kbd "H-s j") 'sp-join-sexp)
(define-key sp-keymap (kbd "H-s s") 'sp-split-sexp)

(add-hook 'before-save-hook 'whitespace-cleanup)

(setq inhibit-splash-screen t)
