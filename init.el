;; Load the package manager
(require 'package)

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; Who am I?
(setq user-full-name "Gaelan D'costa"
      user-mail-address "gdcosta@gmail.com")

;; Add package sources
(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
; (add-to-list 'package-archives
;	     '("marmalade" . "https://marmalade-repo.org/packages/"))

;; Remove unnecessary chrome.
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(fringe-mode 1)

;; It is quicker to type y/n to prompts than "yes" or "no".
(defalias 'yes-or-no-p 'y-or-n-p)

;; Don't ask about following symlinks, just do it.
(setq vc-follow-symlinks t)

;; Isolate custom variables because emacs likes to muck with these, making it hard to manage in source control.
(setq custom-file "~/.emacs.d/custom.el")

;; I use Emacs as my window manager
(require 'exwm)
(setq exwm-input-global-keys
      `(
	;; Bind "s-r" to exit char-mode and fullscreen mode.
	([?\s-r] . exwm-reset)
	;; Bind "s-w" to switch workspace interactively.
	([?\s-w] . exwm-workspace-switch)
	;; Bind "s-p" to launch applications
	;; Bind "s-0 to s-9" to switch to a workspace by its index.
	,@(mapcar (lambda (i)
		    `(,(kbd (format "s-%d" i)) .
		      (lambda ()
			(interactive)
			(exwm-workspace-switch-create ,i))))
		  (number-sequence 0 9))))
;; translate emacs keybindings into CUA-like ones for most apps, since most apps don't observe emacs kebindings and we would like a uniform experience.
(setq exwm-input-simulation-keys
      '(;; movement
	([?\C-b] . [left])
	([?\M-b] . [C-left])
	([?\C-f] . [right])
	([?\M-f] . [C-right])
	([?\C-p] . [up])
	([?\C-n] . [down])
	([?\C-a] . [home])
	([?\C-e] . [end])
	([?\M-v] . [prior])
	([?\C-v] . [next])
	([?\C-d] . [delete])
	([?\C-k] . [S-end delete])
	;; cut/paste
	([?\C-w] . [?\C-x])
	([?\M-w] . [?\C-c])
	([?\C-y] . [?\C-v])
	;; search
	([?\C-s] . [?\C-f])))

;; EXWM buffer names should reflect the name of the window of the X application being run.
(add-hook 'exwm-update-class-hook
	  (lambda ()
	    (exwm-workspace-rename-buffer exwm-class-name)))

;; Set some global window management bindings
(setq exwm-input-global-keys
      `(
	;; 's-r': Reset to (line-mode).
	([?\s-r] . exwm-reset)
	;; 's-w': Switch workspace.
	([?\s-w] . exwm-workspace-switch)
	;; 's-p': Launch application
	([?\s-p] . (lambda (command)
		     (interactive (list (read-shell-command "$ ")))
		     (start-process-shell-command command nil command)))
	;; 's-<N>': Switch to certain workspace.
	,@(mapcar (lambda (i)
		    `(,(kbd (format "s-%d" i)) .
		      (lambda ()
			(interactive)
			(exwm-workspace-switch-create ,i))))
		  (number-sequence 0 9))))
(exwm-enable)

(require 'exwm-randr)
(defun gaelan/exwm-randr-screen-change-hook ()
 (let ((xrandr-output-regexp "\n\\([^ ]+\\) \\(dis\\)?connected ")
       output-connected
       output-disconnected)
       ;; last-output)
   (with-temp-buffer
     (call-process "xrandr" nil t nil)
     (goto-char (point-min))
     (while (re-search-forward xrandr-output-regexp nil 'noerror)
       (if (null (match-string 2))
	   (add-to-list 'output-connected (match-string 1))
	 (add-to-list 'output-disconnected (match-string 1))))
     ;; (princ output-connected)
     ;; (princ output-disconnected)
     (dolist (output output-disconnected)
       (call-process "xrandr" nil nil nil "--output" output "--off"))
     (dolist (output output-connected)
       ;; (princ output)
       (cond ((string= output "DP-1-1")
	      (call-process "xrandr" nil nil nil
			    "--output" output "--primary" "--auto"))
	     ((string= output "DP-1-2")
	      (call-process "xrandr" nil nil nil
			    "--output" output "--auto" "--rotate" "left" "--right-of" "DP-1-1"))
	     ((string= output "eDP-1")
	      (if (= (length output-connected) 1)
		  (call-process "xrandr" nil nil nil
				"--output" output "--primary" "--auto")
		(call-process "xrandr" nil nil nil
			      "--output" output "--off"))))))))
	     ;; (t (call-process "xrandr" nil nil nil
	     ;; 		      "--output" output "--auto" "--right-of" last-output)))
       ;; (setq last-output output)))))
(add-hook 'exwm-randr-screen-change-hook 'gaelan/exwm-randr-screen-change-hook)
(setq exwm-randr-workspace-output-plist
      '(1 "DP-1-2"))
(exwm-randr-enable)
