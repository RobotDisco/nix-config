(require 'cask)
(cask-initialize)

(tool-bar-mode -1)

(line-number-mode t)
(column-number-mode t)

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

(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[gj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

(setq rbenv-installation-dir "/usr/local")
(global-rbenv-mode)

;; Dash at point
(autoload 'dash-at-point "dash-at-point"
  "Search the word at point with Dash." t nil)
(global-set-key (kbd "C-c d") 'dash-at-point)
(global-set-key (kbd "C-c e") 'dash-at-point-with-docset)

(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "M-/") 'hippie-expand)
(sp-use-smartparens-bindings)

(add-hook 'before-save-hook 'whitespace-cleanup)

;; LaTeX configuration
 (setq TeX-parse-self t) ; Enable parse on load.
(setq TeX-auto-save t) ; Enable parse on save.
(setq-default TeX-master nil) ; Query for master file.
(setq TeX-PDF-mode t) ; Use PDF instead of DVI for output

(when (eq system-type 'darwin)
  (setq TeX-view-program-selection
	'((output-dvi "DVI Viewer")
	  (output-pdf "PDF Viewer")
	  (output-html "HTML Viewer")))
  (setq TeX-view-program-list
	'(("DVI Viewer" "open %o")
	  ("PDF Viewer" "open %o")
	  ("HTML Viewer" "open %o"))))

(defun gaelan-kill-other-buffers ()
  "Kill all buffers bu the current one.
Doesn't mess with special buffers."
  (interactive)
  (-each
      (->> (buffer-list)
	(-filter #'buffer-file-name)
	(--remove (eql (current-buffer) it)))
    #'kill-buffer))
(global-set-key (kbd "C-c k") 'gaelan-kill-other-buffers)

(setq inhibit-splash-screen t)
