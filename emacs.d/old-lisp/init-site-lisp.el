;;; init-site-lisp.el --- Non-elpa packages. -*- lexical-binding: t -*-

;;; Commentary:
;;; Set the configuration around packages I have to install manually.
;;; These are packages that don't get installed via the package module.

;;; Code:
(eval-when-compile (require 'cl-lib))

(defun gaelan/add-subdirs-to-load-path (parent-dir)
  "Add every non-hidden subdir of PARENT-DIR to `load-path'."
  (let* ((default-directory parent-dir))
    (progn
      (setq load-path
	    (append
	     (cl-remove-if-not
	      (lambda (dir) (file-directory-p dir))
	      (directory-files (expand-file-name parent-dir) t "^[^\\.]"))
	     load-path)))))

(gaelan/add-subdirs-to-load-path
 (expand-file-name "site-lisp/" user-emacs-directory))

;;;---------------------------------------------------------------------------
;;; Utilities for grabbing upstream libs
;;;---------------------------------------------------------------------------
(defun gaelan/site-lisp-dir-for (name)
  "Given a package NAME, give back the filesystem path."
  (expand-file-name (format "site-lisp/%s" name) user-emacs-directory))

(defun gaelan/site-lisp-library-el-path (name)
  "Given a single-file package NAME, give back filesystem path."
  (expand-file-name (format "%s.el" name) (gaelan/site-lisp-dir-for name)))

(defun gaelan/download-site-lisp-module (name url)
  "Download package NAME from URL and place in site-lisp path."
  (let ((dir (gaelan/site-lisp-dir-for name)))
    (message "Downloading %s from %s" name url)
    (unless (file-directory-p dir)
      (make-directory dir t))
    (add-to-list 'load-path dir)
    (let ((el-file (gaelan/site-lisp-library-el-path name)))
      (url-copy-file url el-file t nil)
      el-file)))

(defun gaelan/recompile-lib-from-url (name url)
  "compile package NAME from URL."
  (byte-compile-file (gaelan/download-site-lisp-module name url)))

(defun gaelan/ensure-lib-from-url (name url)
  "Download and compile package NAME from URL unless already loadable."
  (unless (gaelan/site-lisp-library-loadable-p name)
    (gaelan/recompile-lib-from-url name url)))

(defun gaelan/site-lisp-library-loadable-p (name)
  "Is library NAME loadable from source file under `site-lisp/name/'?"
  (let ((f (locate-library (symbol-name name))))
    (and f (string-prefix-p (file-name-as-directory (gaelan/site-lisp-dir-for name)) f))))

(provide 'init-site-lisp)
;;; init-site-lisp.el ends here
