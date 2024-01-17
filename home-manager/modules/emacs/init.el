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

(menu-bar-mode t)

(setq custom-file "~/.config/emacs/custom.el")
(load custom-file)


(setq inhibit-startup-screen t)

(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta
	mac-option-modifier 'super))


;; Always use Y/N prompts instead of "yes"/"no".
(defalias 'yes-or-no-p 'y-or-n-p)


(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)


(eval-when-compile
  (require 'use-package))
;; (require 'diminish) ; If I use the :diminish keyword
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
  :hook (org-mode . (lambda ()
			   (setq-local fill-column 80)
			   (visual-line-mode +1)))
  :bind (("C-c l" . org-store-link)
	 ("C-c a" . org-agenda)
	 ("C-c c" . org-capture))
  :config
  (add-to-list 'org-modules 'org-habit t)
  :custom
  (org-agenda-files '("~/Documents/brain/gtd/inbox.org"
		      "~/Documents/brain/gtd/gtd.org"
		      "~/Documents/brain/gtd/tickler.org"))
  (org-capture-templates 
   '(("t" "New TODO" entry (file "~/Documents/brain/gtd/inbox.org")
      "* TODO %?")
     ("p" "New PROJECT" entry (file "~/Documents/brain/gtd/inbox.org")
      "* PLAN %? :PROJECT:")))
  (org-refile-targets
   '(("~/Documents/brain/gtd/gtd.org" . (:level . 1))
     ;; Maybe I want to move a TODO into a project, which is itself at level 1?
     ;; But I don't want to see non-project tasks.
     ("~/Documents/brain/gtd/gtd.org" . (:tag . "PROJECT"))
     ("~/Documents/brain/gtd/someday.org" . (:level . 1))
     ("~/Documents/brain/gtd/tickler.org" . (:level . 1))))
  (org-todo-keywords
   '((sequence
      "TODO(q)" "STARTED(e!)" "WAITING(r@/!)" "|" "DONE(t!)" "CANCELLED(y@/!)")
     (sequence
      "PLAN(z)" "ACTIVE(x!)" "PAUSED(s@/!)" "RETRO(c)" "|" "COMPLETED(v!)" "ABANDONED(a@/!)")))
  (org-tag-alist
   '((:startgroup)
     ("Contexts")
     (:grouptags)
     ("@home" . ?h) ("@lappy" . ?l) ("@online" . ?n) ("@phone" . ?p) ("@errand" . ?e)
     (:endgroup)
     (:startgroup) ("Focus") (:grouptags) ("DEEP" . ?d) ("SHALLOW" . ?s) (:endgroup)
     ("WORK") ("PROJECT")))
  (org-priority-highest ?A)
  (org-priority-default ?D)
  (org-priority-lowest ?D)
  (org-log-done 'time)
  (org-enforce-todo-dependencies t)
  (org-tags-exclude-from-inheritance '(resource area)))


  (use-package org-roam
    :ensure t
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
	:target (file+head "literature/${citar-citekey}.org" "#+TITLE: ${citar-author} (${citar-date}) - ${citar-title}\n#+FILETAGS: literature fleeting\n")
	:unnarrowed t)
       ("p" "permanent" plain "%?"
	:target (file+head "permanent/%<%Y%m%d%H%M%S>-${slug}.org"
			   "#+TITLE: ${title}")
	:unnarrowed t)
       ("r" "rolodex" plain "%?"
	:target (file+head "rolodex/%<%Y%m%d%H%M%S>-${slug}.org"
			   "#+TITLE: ${title}"))
       ("j" "project" plain
	(file "~/Documents/brain/templates/project.org")
	:target (file+head "project/%<%Y%m%d%H%M%S>-${slug}.org"
			   "#+TITLE: #{title}"))))
    (org-roam-directory "~/Documents/brain")
    (org-roam-dailies-directory "journal/")
    (org-roam-dailies-capture-templates
     '(("j" "Daily Log Entry" entry
	"* %<%0H:%M> %?"
	:target (file+head+olp "%<%Y>/%<%0m>/%<%Y-%m-%d>.org" "#+SETUPFILE: ../../settings.org\n#+TITLE: %<%Y-%0m-%0d, %A>\n#+FILETAGS: :fleeting:" ("Log"))
	:unnarrowed t)
       ("d" "Daily Morning Reflection" entry
	(file "~/Documents/brain/templates/daily.org")
	:jump-to-captured t
	:prepend t
	:target (file+head "%<%Y>/%<%0m>/%<%Y-%0m-%0d>.org" "#+SETUPFILE: ../../settings.org\n#+TITLE: %<%Y-%0m-%0d, %A>\n#+FILETAGS: :fleeting:"))
       ("e" "Daily Evening Reflection" entry
	(file "~/Documents/brain/templates/evening.org")
	:target (file+head "%<%Y>/%<%0m>/%<%0Y-%0m-%0d>.org" "#+SETUPFILE: ../../settings.org\n#+TITLE: %<%Y-%0m-%0d, %A>\n#+FILETAGS: :fleeting:")
	:unnarrowed t)
       ("w" "Weekly Reflection" entry
	(file "~/Documents/brain/templates/weekly.org")
	:target (file+head "%<%Y>/%<%0m>/%<%0Y-%0m-%0d>.org" "#+SETUPFILE: ../../settings.org\n#+TITLE: %<%Y-%0m-%0d, %A>\n#+FILETAGS: :fleeting:")
	:unnarrowed t)
       ("m" "Monthly Reflection" entry
	(file "~/Documents/brain/templates/monthly.org")
	:target (file+head "%<%Y>/%<%0m>/%<%Y-%0m-%0d>.org" "#+SETUPFILE: ../../settings.org\n#+TITLE: %<%Y-%0m-%0d, %A>\n#+FILETAGS: :fleeting:")
	:unnarrowed t)
       ("y" "Yearly Reflection" entry
	(file "~/Documents/brain/templates/yearly.org")
	:target (file+head "%<%Y>/%<%0m>/%<%Y-%0m-%0d>.org" "#+SETUPFILE: ../../settings.org\n#+TITLE: %<%Y-%0m-%0d, %A>\n#+FILETAGS: :fleeting:")
	:unnarrowed t)))
    :config
    (org-roam-db-autosync-mode))

(use-package citar
  :ensure t
  :custom
  (citar-bibliography '("~/Documents/brain/literature/REFERENCES.bib")))

(use-package citar-org-roam
  :ensure t
  :after (citar org-roam)
  :custom
  (citar-bibliography '("~/Documents/brain/literature/REFERENCES.bib"))
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
  (setq-local fill-column 80)
  :config
  (global-auto-revert-mode +1))
