;;; init-appearaance.el -- basic cosmetic emacs configuration. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;; Remove unnecessary chrome.
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(fringe-mode 1)

;; Show columns
(column-number-mode +1)

;; Prettify necessary chrome
(gaelan/require-package 'rebecca-theme)

(if (daemonp)
    ;; We need this hack because when you initialize emacs as a daemon,
    ;; no frame is created so a lot of important theme loading computations
    ;; do not get run. However, this is especially hacky because we don't
    ;; want to reload the theme from scratch on every frame creation but
    ;; that's the only hook we can do this, so our hook has to remove itself
    ;; when it is done.
    (cl-labels ((load-my-theme (frame)
			       (with-selected-frame frame
				 (load-theme 'rebecca t))
			       (remove-hook 'after-make-frame-functions #'load-my-theme)))
      (add-hook 'after-make-frame-functions #'load-my-theme))
  (load-theme 'rebecca t))

(add-to-list 'default-frame-alist '(font . "Anonymous Pro-14"))

(provide 'init-appearance)
;;; init-appearance.el ends here

