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

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

(customize-save-variable 'org-log-into-drawer t)
(customize-save-variable 'org-todo-keywords
			 '((sequence "TODO" "NEXT" "WAITING" "|" "DONE" "CANCELLED")
			   (sequence "TRIAGE" "PROJECT" "SOMEDAY" "DONE")))
(customize-save-variable 'org-tag-persistent-alist
			 '((:startgroup . nil)
			   ("@home" . ?h)
			   ("@officekw" . ?k)
			   ("@officeto" . ?t)
			   ("@errands" . ?e)
			   ("@phone" . ?p)
			   ("@lappy" . ?l)
			   ("@online" . ?o)
			   ("@brain" . ?b)
			   (:endgroup . nil)))
(customize-save-variable 'org-agenda-files
			 `(,(concat gaelan/webdav-prefix "gtd/gtd.org")
			   ,(concat gaelan/webdav-prefix "gtd/someday.org")
			   ,(concat gaelan/webdav-prefix "gtd/tickler.org")
			   ,(concat gaelan/webdav-prefix "gtd/gcal-personal.org")
			   ,(concat gaelan/webdav-prefix "gtd/gcal-work.org")
			   ,(concat gaelan/webdav-prefix "gtd/inbox.org")))

(provide 'gaelan/init-org)
