;;; init-bitwarden.el -- bitwarden integration. -*- lexical-binding: t -*-

;;; Commentary:
;;; Bitwarden integration

;;; Code:
(require 'init-site-lisp)

(let ((bw-package-symbol 'bitwarden)
      (bw-url "https://raw.githubusercontent.com/RobotDisco/emacs-bitwarden/master/bitwarden.el"))

  (gaelan/ensure-lib-from-url bw-package-symbol bw-url)

  (defun gaelan/bitwarden-redownload-plugin ()
    "Redownload and recompile latest version of bitwarden plugin."
    (gaelan/recompile-lib-from-url bw-package-symbol bw-url))

  (when (require 'bitwarden nil t)
    (customize-set-variable 'bitwarden-user "gdcosta+bitwarden@gmail.com")))

(provide 'init-bitwarden)
;;; init-bitwarden.el ends here
