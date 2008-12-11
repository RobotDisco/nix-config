; This is where we store our custom plugins
(add-to-list 'load-path "~/.elisp/ljupdate")
(add-to-list 'load-path "~/.elisp/color-theme-6.6.0")

; Load these at startup
(require 'ljupdate)
(require 'color-theme)

; Why spaces over tabs?
; 1) http://www.jwz.org/doc/tabs-vs-spaces.html
; 2) Try writing Haskell code using only tabs
; 3) Tabs eventually will screw people up
; That being said, I prefer my indents mod 2
(setq default-tab-width 2)
(setq indent-tab-mode nil)

; Make emacs pretty!
(color-theme-initialize)
(color-theme-subtle-hacker)

(custom-set-variables
 ;; custom-set-variables was added by Custom -- don't edit or cut/paste it!
 ;; Your init file should contain only one such instance.
 '(case-fold-search t)
 '(current-language-environment "English")
 '(global-font-lock-mode t nil (font-lock))
 '(save-place t nil (saveplace))
 '(show-paren-mode t nil (paren))
 '(transient-mark-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom -- don't edit or cut/paste it!
 ;; Your init file should contain only one such instance.
 )
