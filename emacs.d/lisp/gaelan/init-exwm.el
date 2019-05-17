;;; EXWM

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

(defun gaelan/exwm-randr-screen-change-hook ()
  ;; Start by enumerating over which screens are connected and disconnected
  (let ((xrandr-output-regexp "\n\\([^ ]+\\) \\(dis\\)?connected ")
       output-connected
       output-disconnected)
   (with-temp-buffer
     (call-process "xrandr" nil t nil)
     (goto-char (point-min))
     (while (re-search-forward xrandr-output-regexp nil 'noerror)
       (if (null (match-string 2))
	   (add-to-list 'output-connected (match-string 1))
	 (add-to-list 'output-disconnected (match-string 1))))
     ;; disable all screens that are marked as disabled.
     (dolist (output output-disconnected)
       (call-process "xrandr" nil nil nil "--output" output "--off"))
     (dolist (output output-connected)
       (cond ((string= output "DP-1-1")
	      ;; When docked, this is my main monitor
	      (call-process "xrandr" nil nil nil
			    "--output" output "--primary" "--auto"))
	     ((string= output "DP-1-2")
	      ;; My second monitor is in portrait mode
	      (call-process "xrandr" nil nil nil
			    "--output" output "--auto" "--rotate" "left" "--right-of" "DP-1-1"))
	     ((string= output "eDP-1")
	      (if (= (length output-connected) 1)
		  ;; If this is the only connected screen, mark it as the primary one.
		  (call-process "xrandr" nil nil nil
				"--output" output "--primary" "--auto")
		;; If it isn't the only monitor, my laptop is most likely in
		;; clamshell mode.
		(call-process "xrandr" nil nil nil
			      "--output" output "--off"))))))))

(require 'exwm-randr)
(setq exwm-randr-workspace-output-plist
      '(1 "DP-1-2"))
(add-hook 'exwm-randr-screen-change-hook 'gaelan/exwm-randr-screen-change-hook)
(exwm-randr-enable)

(provide 'gaelan/init-exwm)
