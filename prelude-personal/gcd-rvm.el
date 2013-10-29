;;; gcd-rvm.el --- Emacs Prelude (Personal): Adding RVM support to Prelude
;;
;; Copyright © 2013-2014 Gaelan D'costa
;;
;; Author: Gaelan D'costa <gdcosta@gmail.com>
;; URL: https://github.com/NaleagDeco/prelude
;; Created: August 2013
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Rather than loading RVM by default, why not only activate it when I'm
;; reading a Ruby file? Chances are I'm using RVM for all projects anyway.
;; Following the patterns I've (hopefully) understood from Prelude core.

;;; Code:

(eval-after-load 'ruby-mode
  '(progn
     (defun personal-rvm-defaults ()
       (prelude-require-package 'rvm)
       (rvm-activate-corresponding-ruby))

     (setq personal-rvm-hook 'personal-rvm-defaults)
     (add-hook 'ruby-mode-hook (lambda ()
                                 (run-hooks 'personal-rvm-hook)))))

(provide 'gcd-rvm)
;;; gcd-rvm.el ends here
