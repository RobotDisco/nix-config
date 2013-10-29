;;; gcd-solarized.el --- Emacs Prelude (Personal): Solarized on start
;;
;; Copyright © 2013 Gaelan D'costa
;;
;; Author: Gaelan D'costa <gdcosta@gmail.com>
;; URL: https://github.com/NaleagDeco/prelude
;; Created: August 2013
;; Keywords: convenience

;;; Commentary:

;; Solarized is prettier than Zenburn (the default Prelude theme), so let's
;; use it instead.

;;; Code:

(prelude-require-package 'solarized-theme)
(load-theme 'solarized-dark t)

(provide 'gcd-solarized)
;;; gcd-solarized.el ends here
