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
			 '((sequence "TODO(t)" "NEXT(n)" "WAITING(w)" "|" "DONE(d)")
			   (sequence "TRIAGE(r)" "PROJECT(p)" "SOMEDAY(s)" "|" "COMPLETED(c)")
			   (sequence "|" "CANCELLED(a)")))
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
			   ,(concat gaelan/webdav-prefix "gtd/gcal/personal.org")
			   ,(concat gaelan/webdav-prefix "gtd/gcal/work.org")
			   ,(concat gaelan/webdav-prefix "gtd/inbox.org")))

(when (require 'bitwarden nil t)
  (customize-save-variable 'bitwarden-data-file
			   (unless *is-osx*
			     "~/.config/Bitwarden CLI/data.json"))
  (customize-save-variable 'bitwarden-user "gdcosta+bitwarden@gmail.com"))

(when (require 'org-gcal nil t)
  (let* ((bwdata (elt (bitwarden-search "offlineimap") 0))
	 (bwfields (gethash "fields" bwdata))
	 (client-id (gethash "value" (elt bwfields 0)))
	 (client-secret (gethash "value" (elt bwfields 1))))
    (customize-save-variable 'org-gcal-client-id client-id)
    (customize-save-variable 'org-gcal-client-secret client-secret))
  (customize-save-variable 'org-gcal-file-alist
			   `(("gdcosta@gmail.com" . ,(concat gaelan/webdav-prefix "gtd/gcal/personal.org"))
			     ("gaelan@tulip.com" . ,(concat gaelan/webdav-prefix "gtd/gcal/work.org")))))

(provide 'gaelan/init-org)
