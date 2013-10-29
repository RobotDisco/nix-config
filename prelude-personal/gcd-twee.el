;;; gcd-twee.el --- Enabling Twee Mode for twine files.
;;
;; Copyright © 2013 Gaelan D'costa
;;
;; Author: Gaelan D'costa <gdcosta@gmail.com>
;; URL: http://github.com/NaleagDeco/prelude
;; Created: August 2013
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Enable the use of the independently-downloaded Twee mode for twine files.

;;; Code:

(autoload 'twee-mode "twee.el" "Mode for editing Twee files." t)
(add-to-list 'auto-mode-alist '("\\.tw\\'" . twee-mode))
(add-hook 'twee-mode-hook 'turn-on-font-lock)

(provide 'gcd-twee)
;;; gcd-twee.el ends here
