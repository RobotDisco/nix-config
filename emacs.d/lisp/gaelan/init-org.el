;;; Org mode

;; All my org files live in a cloud-synced directory that differ between OSX and Linux
(setq gaelan/webdav-prefix
      (if (eql system-type 'darwin)
	  (file-name-as-directory "~/Seafile/emacs/")
	(file-name-as-directory "~/fallcube/emacs/")))

(when (require 'org-journal nil t)
  (customize-save-variable 'org-journal-dir
			   (file-name-as-directory (concat gaelan/webdav-prefix "journal/")))
  (customize-save-variable 'org-journal-file-format "%Y/%Y%m%d.org")
  ;; Bullet Journal discourages carrying over todos. Decide that explicitly!
  (customize-save-variable 'org-journal-carryover-items nil))

;; Prettify org mode, remove unnecessary asterix.
(when (require 'org-bullets nil t)
  (add-hook 'org-mode-hook 'org-bullets-mode))

(provide 'gaelan/init-org)
