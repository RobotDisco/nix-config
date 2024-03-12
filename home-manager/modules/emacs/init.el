;; Print startup time via a hook so that the message isn't clobbered other
;; messages
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (message "Emacs ready in %s with %d garbage collections."
		     (format "%.2f seconds."
			     (float-time
			      (time-subtract after-init-time
					     before-init-time)))
		     gcs-done)))

(setq custom-file "~/.config/emacs/custom.el")
(load custom-file)


(setq inhibit-startup-screen t)

(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta
	mac-option-modifier 'super))

;; Remove unnecessary chrome
(menu-bar-mode -1)
(tool-bar-mode -1)

;; Add colum to modeline
(column-number-mode +1)


;; Always use Y/N prompts instead of "yes"/"no".
(defalias 'yes-or-no-p 'y-or-n-p)


(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)


(eval-when-compile
  (require 'use-package))
(require 'diminish) ; If I use the :diminish keyword
;; (require 'delight) ; If I use the :delight keyword maybe?
;; (require 'bind-key) ; If I use the :bind keyword

(use-package emacs
  :defer 2
  :bind (("C-c C-w C-p" . winner-undo)
	 ("C-c C-w C-n" . winner-redo))
  :config
  (winner-mode +1))

(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'")


(use-package yaml-mode
  :ensure t
  :mode "\\.ya?ml\\'")

(use-package org
  :ensure t
  :hook ((org-mode . (lambda ()
		      (setq-local fill-column 80)
		      (visual-line-mode +1))))
  :bind (("C-c a" . gaelan-org-agenda)
	 ("C-c l" . org-store-link)
	 ("C-c c" . org-capture))
  :config
  (add-to-list 'org-modules 'org-habit t)
  :init
  (defun gaelan-org-agenda ()
    (interactive)
    (require 'org-roam)
    (org-agenda))
  :custom
  (org-agenda-prefix-format
   '((agenda . " %i %(gaelan-agenda-category 12)%?-12t% s")
     (todo . " %i %(gaelan-agenda-category 12)")
     (tags . " %i %(gaelan-agenda-category 12)")
     (search . " %i %(gaelan-agenda-category 12)")))
  (org-archive-location "~/Documents/brain/gtd/archive/archive.org::datetree/")
  (org-enforce-todo-dependencies t)
  (org-global-properties
   '(("EFFORT_ALL" . "0 0:05 0:15 0:30 1:00 2:00 4:00 8:00")))
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-priority-highest ?A)
  (org-priority-default ?D)
  (org-priority-lowest ?D)
  (org-refile-targets '((org-agenda-files . (:tag . "GTD"))
			(org-agenda-files . (:tag . "SOMEDAY"))
  			(org-agenda-files . (:tag . "TICKLER"))))
  (org-stuck-projects '("project" ("TODO" "NEXT" "DOING") nil ""))
  (org-tag-alist '((:startgroup)
		   ("DEEP")
		   ("SHALLOW")
		   ("RELAX")
		   (:endgroup)
		   ("tulip")
		   ("@lappy")
		   ("@online")
		   ("@phone")
		   ("@brain")))
  (org-tags-exclude-from-inheritance '("project" "TODOS" "area"))
  (org-todo-keywords
   '((sequence
      "TRIAGE" "TODO(q)" "NEXT(w!)" "DOING(e!/!)" "WAITING(r@/@)" "|" "DONE(t!)" "CANCELLED(y@)")
     (sequence
      "PLAN(z)" "ACTIVE(x!/!)" "RETRO(c)" "PAUSED(v@/!)" "|" "COMPLETED(b!)" "ABANDONED(n@)"))))


  (use-package org-roam
    :ensure t
    :hook ((find-file . gaelan-roam-todos-update-file)
	    (before-save . gaelan-roam-todos-update-file))
    :bind (("C-c j d" . (lambda ()
			  (interactive)
			  (org-roam-dailies-goto-date nil "j")))
	   ("C-c j D" . org-roam-dailies-capture-date)
	   ("C-c j t" . (lambda ()
			  (interactive)
			  (org-roam-dailies-goto-today "j")))
	   ("C-c j T" . org-roam-dailies-capture-today)

	   ;; Buttonmash combo for logging a log
	   ;; doesn't match my mnenomic scheme otherwise.
	   ("C-c j j" . (lambda ()
			  (interactive)
			  (org-roam-dailies-capture-today nil "j")))

	   ("C-c j y" . (lambda (n)
			  (interactive "p")
			  (org-roam-dailies-goto-yesterday n "j")))
	   ("C-c j Y" . org-roam-dailies-capture-yesterday)
	   ("C-c j r" . (lambda (n)
			  (interactive "p")
			  (org-roam-dailies-goto-tomorrow n "j")))
	   ("C-c j R" . org-roam-dailies-capture-tomorrow)
	   ("C-c j n" . org-roam-dailies-goto-next-note)
	   ("C-c j p" . org-roam-dailies-goto-previous-note)
	   ("C-c n b" . org-roam-buffer-toggle)
	   ("C-c n c" . org-id-get-create)
	   ("C-c n f" . org-roam-node-find)
	   ("C-c n i" . org-roam-node-insert)
	   ("C-c n F" . (lambda ()
			  (interactive)
			  (org-roam-node-random nil (lambda (node)
						      (member "fleeting"
							      (org-roam-node-tags mode))))))
	   ("C-c n R" . (lambda ()
			  (interactive)
			  (org-roam-node-random nil (lambda (node)
						      (and
						       (member "journal" (org-roam-node-tags node))
						       (> (org-roam-node-level node) 0)))))))
    :custom
    (org-roam-capture-templates
     '(("l" "literature" plain
	(file "~/Documents/brain/templates/literature.org")
	:target (file "${citar-citekey}.org")
	:unnarrowed t)
       ("p" "permanent" plain "%?"
	:target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
			   "#+TITLE: ${title}")
	:unnarrowed t)
       ("r" "rolodex" plain "%?"
	:target (file+head "%<%Y%m%d%H%M%S>-${slug}.org.gpg"
			   "#+TITLE: ${title}"))
       ("j" "project" plain
	(file "~/Documents/brain/templates/project.org")
	:target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
			   "#+TITLE: #{title}"))))
    (org-roam-directory "~/Documents/brain")
    (org-roam-dailies-directory "journal/")
    (org-roam-dailies-capture-templates
     '(("j" "Daily Log Entry" entry
	"* %<%0H:%M> %?"
	:target (file+head+olp "%<%Y>/%<%0m>/%<%Y-%m-%d>.org" "#+TITLE: %<%Y-%0m-%0d, %A>\n#+FILETAGS: :journal:fleeting:" ("Log"))
	:unnarrowed t)
       ("d" "Daily Morning Reflection" entry
	(file "~/Documents/brain/templates/daily.org")
	:jump-to-captured t
	:prepend t
	:target (file+head "%<%Y>/%<%0m>/%<%Y-%0m-%0d>.org" "#+TITLE: %<%Y-%0m-%0d, %A>\n#+FILETAGS: :journal:fleeting:"))
       ("e" "Daily Evening Reflection" entry
	(file "~/Documents/brain/templates/evening.org")
	:target (file+head "%<%Y>/%<%0m>/%<%0Y-%0m-%0d>.org" "#+TITLE: %<%Y-%0m-%0d, %A>\n#+FILETAGS: :journal:fleeting:")
	:unnarrowed t)
       ("w" "Weekly Reflection" entry
	(file "~/Documents/brain/templates/weekly.org")
	:target (file+head "%<%Y>/%<%0m>/%<%0Y-%0m-%0d>.org" "#+TITLE: %<%Y-%0m-%0d, %A>\n#+FILETAGS: :journal:fleeting:")
	:unnarrowed t)
       ("m" "Monthly Reflection" entry
	(file "~/Documents/brain/templates/monthly.org")
	:target (file+head "%<%Y>/%<%0m>/%<%Y-%0m-%0d>.org" "#+TITLE: %<%Y-%0m-%0d, %A>\n#+FILETAGS: :journal:fleeting:")
	:unnarrowed t)
       ("y" "Yearly Reflection" entry
	(file "~/Documents/brain/templates/yearly.org")
	:target (file+head "%<%Y>/%<%0m>/%<%Y-%0m-%0d>.org" "#+TITLE: %<%Y-%0m-%0d, %A>\n#+FILETAGS: :journal:fleeting:")
	:unnarrowed t)))
    (org-roam-node-display-template
     ;; Concatting allows vertico to separate fields out
     ;; in a nice column-based way
     (concat "${title:*} "
	     (propertize "${tags:20}" 'face 'org-tag)))
    :config
    (defun gaelan-agenda-files ()
      "Dynamically list every org-roam node that we expect TODOs in.

Currently this is just any node that is tagged as an :area:."
      ;; Get the string out of each one-item list.
      (mapcar #'car
	      ;; join nodes to tags table to get files of all nodes that have area tag.
	      (org-roam-db-query [:select [nodes:file]
					  :from tags
					  :left-join nodes
					  :on (= tags:node-id nodes:id)
					  :where (= tag "TODOS")])))

    ;; Functions I've written for custom behaviour. Stolen/inspired from a bunch of sources:
    ;; https://d12frosted.io/posts/2020-06-24-task-management-with-roam-vol2.html
    ;; https://d12frosted.io/posts/2020-06-24-task-management-with-roam-vol5.html
    (defun gaelan-agenda-files-update (&rest _)
      "Hook that dynamically sets org-agenda-files list based on org-roam query."
      (setq org-agenda-files (gaelan-agenda-files)))

    (defun gaelan-agenda-category (&optional len)
      "Return the title of the org file.

If LEN is supplied, truncate the string to those many characters."
      ;; If there are subheadings in an org file, we want the first/root
      ;; heading anyway.
      (org-with-point-at (point-min)
	(if-let* ((str (or
		       (gaelan-org-buffer-keyword-get "TITLE")
		       (org-get-heading t t t t)
		       (if-let (filename (buffer-file-name))
			   (file-name-base filename))))
		 (cstr (concat str ":")))
	    (if len
		(string-pad cstr len)
	       cstr))))

    (defun gaelan-org-buffer-keyword-get (name)
      "Get a buffer keyword called NAME as a string.

Note that this will obey inheritance from a keyword like #+SETUPFILE:"
      (when-let* ((namelist (list name))
		  (alist (assoc name
			       (org-collect-keywords namelist))))
	(cdr (assoc name (org-collect-keywords (list name) (list name))))))

    (defun gaelan-has-todos-p ()
      "Return non-nil if current-buffer contains uncompleted todo entries.

Note that we are explicitly ignoring headlines that are in a completed state
(i.e. have a :todo-type value of 'done) and entries that aren't todos at all
(i.e. are missing a :todo-type property).

This function only cares about the presence of even a single qualifying todo."
      (org-element-map
	  (org-element-parse-buffer 'headline)
	  'headline
	(lambda (h)
	  (eq (org-element-property :todo-type h)
	      'todo))
	nil 'first-match))

    (defun gaelan-roam-todos-update-file ()
      "If current buffer is an org-roam file, tag it based on presence of todos.

If any uncompleted todos are found, add a :todos: tag if not present.
If there are no uncompleted todos in the file, remove any :todos: tag."
      (unless (or (not (org-roam-file-p))
		  (active-minibuffer-window))
	(save-excursion
	  ;; Go to the beginning of the file to insert any tags as a FILETAG.
	  (goto-char (point-min))
	  (when-let* ((node (org-roam-node-at-point))
		      (tags (org-roam-node-tags node)))
	    (if (gaelan-has-todos-p)
		(unless (member "TODOS" tags)
		  (org-roam-tag-add '("TODOS")))
	      (when (member "TODOS" tags)
		(org-roam-tag-remove '("TODOS"))))))))
    ;; We use a dynamic hook to populate org-agenda-files whenever we view our org-agenda.
    (advice-add 'org-agenda :before #'gaelan-agenda-files-update)
    (org-roam-db-autosync-mode))

(use-package citar-org-roam
  :diminish citar-org-roam
  :ensure t
  :after (org-roam)
  :custom
  (citar-bibliography '("~/Documents/brain/REFERENCES.bib"))
  (citar-org-roam-note-title-template "${author} (${date}) - ${title}")
  (citar-org-roam-capture-template-key "l")
  :config
  (citar-org-roam-mode))

;; The default completions in emacs 28 are as follows:

;; basic :: foo|bar looks for all completions that have foo at the
;; front, bar at the end. It matches "foo*bar"

;; emacs22 :: foo|bar looks for all foo* candidates then shoves var
;; onto the end.

;; partial-completion can complete multiple words, which one given as
;; a prefix. It will only match completions with matching prefix in
;; the order of the prefixes that were given. /u/mo/s will only match
;; phrases that look like "/usr/monnier/src" not "/monier/user/src"

;; So we add orderless, which allows us to complete against multiple
;; words, but in any order. For example, matching against /u/mo/s will
;; match both "/usr/monier/src" and "/usr/src/monier"
;;
;; There are cases where orderless won't work, so we use basic as a fallback.
;;
;; Tramp mode doesn't handle orderless well (because it involves
;; iterating over directory tree combination across the wire, so
;; specifically for file navigation we try for basic first and
;; partial-completion after so we can get partial intelligence without
;; expending unnecessary effort.
(use-package orderless
  :ensure t
  :defer 2
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; Vertico isn't the regular tab-based completion.  It pops up a menu,
;; and you can navigate up and down.  A nice thing about it is you can
;; use C-p and C-n to pick the candidate you want instead of
;; contorting your input to match what you want.
(use-package vertico
  :ensure t
  :defer 2
  :after (orderless)
  :config
  (vertico-mode))

(use-package emacs
  :defer 2
  :hook (program-mode . (lambda ()
			  (auto-fill-mode + 1)))
  :custom
  ;; This ID corresponds to my key for gdcosta@gmail.com
  (epa-file-encrypt-to '("A815AC9D526EE85A"))
  (epg-pinentry-mode 'loopback)
  (fill-column 80)
  :config
  (global-auto-revert-mode +1))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
	 ("C-c g" . magit-dispatch)
	 ("C-c f" . magit-file-dispatch))
  :custom
  (magit-wip-mode t))

(use-package keychain-environment
  :ensure t
  :defer 2
  :config
  (keychain-refresh-environment))
