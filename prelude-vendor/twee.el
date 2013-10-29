;; Requirements
;; ------------

;; You'll need to have an emacs that has generic mode support (all modern
;; ones should) as well as longlines.el installed. You can grab longlines
;; online at:

;; http://www.emacswiki.org/cgi-bin/wiki/longlines.el


;; Installation
;; ------------

;; Place both longlines.el and twee.el into your load path -- if you're
;; not sure, it's probably best to put it into your site-lisp
;; directory. (If you don't know where that is, try locate site-lisp.)
;; Then add these lines to your .emacs file:

;; (autoload 'twee-mode "twee.el" "Mode for editing Twee files." t)
;; (add-to-list 'auto-mode-alist '("\\.tw\\'" . twee-mode))
;; (add-hook 'twee-mode-hook 'turn-on-font-lock)

;; That's it. If all's right in the world, you'll pop into Twee mode
;; whenever you start editing a file whose name ends in .tw. 

;; -- Chris Klimas
;; klimas@gmail.com


;; Change History
;; --------------

;; October 18, 2006 [[ConalElliott]]
;; * syntax highlighting regexps simplified, improved, and commented 
;; * new highlighting: WikiWord, !!Subheading, {{{monospace}}}, /%comment%/
;; * key-bindings for markup insertion
;; 
;; December 19, 2006 [[ConalElliott]]
;; * Tweaked WikiWord regexp: added word boundaries ("\<...\>")
;; December 26, 2006 [[ConalElliott]]
;; * paragraph-start and paragraph-separate
;; December 27, 2006 [[ConalElliott]]
;; * twee-add-item (\C-ci): add a numbered or bulletted item.
;; January 3, 2007 [[ConalElliott]]
;; * twee-add-sublist (\C-cI): add a new sublist with NestedSlider.
;; January 4, 2007 [[ConalElliott]]
;; * page-delimiter
;; * twee-no-longlines, and uses


;; Code
;; ----

(require 'longlines)

;; Basic mode definition
(define-generic-mode 'twee-mode
  nil   
  nil 
  '(("^::.*"  . font-lock-function-name-face)            ; :: Title [tags]
    ("^!.*" . font-lock-keyword-face)                    ; !!Subheading
    ("\<[A-Z][a-zA-Z_]*[a-z_][A-Z][a-zA-Z_]*\>"          ; WikiWord
     . font-lock-function-name-face)
    ;; conal: the regular expressions below are problematic.  For
    ;; instance, the italics pattern is a pair of slashes, followed by
    ;; non-slashes, followed by another pair of slashes.  So a *single*
    ;; slash mistakenly ends the italic formatting.  I don't know how to
    ;; write a regexp to describe a sequence of characters without two
    ;; consecutive slashes.  Moreover, a "http://" will trigger italic
    ;; mode.  I guess we need some context-sensitivity to do highlighting
    ;; robustly.
    ;; To do: try a face with actual bold & italics for the bold & italic
    ;; markup.
    ("''[^']*''" . font-lock-keyword-face)               ; ''bold''
    ("\\[\\[[^]]*\\]\\]" . font-lock-function-name-face) ; [[tag]]
    ("//[^/]*//" . font-lock-keyword-face)               ; //italics//
    ("<<[^>]*>>" . font-lock-warning-face)               ; <<macro>>
    ("{{{[^}]*}}}" . font-lock-constant-face)            ; {{{monospace}}}
    ("/%[^%]*%/" . font-lock-comment-face)               ; /%comment%/
    )
  '("\\.tw\\'")
  (list 'twee-mode-setup-function)
  "Major mode for editing Twee files.")  

(defvar twee-mode-hook nil)

(defun twee-mode-setup-function ()
  (longlines-mode)
  ;; Key bindings for easy insertion of formatting
  (local-set-key "\C-c[" (twee-delim-command "[[" "]]"))
  (local-set-key "\C-c<" (twee-delim-command "<<" ">>"))
  (local-set-key "\C-c{" (twee-delim-command "{{{" "}}}"))
  (local-set-key "\C-c'" (twee-delim-command "''" "''"))
  (local-set-key "\C-c/" (twee-delim-command "//" "//"))
  (local-set-key "\C-ci" 'twee-add-item)
  (local-set-key "\C-cI" 'twee-add-sublist)
  (set (make-local-variable 'paragraph-start) (concat "^$\\|" page-delimiter))
  (set (make-local-variable 'paragraph-separate) paragraph-start)
  ;; Use tiddler name lines for page delimiter.  Not quite what I really
  ;; want.  I'd love to have a command like mark-page that grabs the
  ;; tiddler I'm editing (without name), so I can cut & paste.
  (set (make-local-variable 'page-delimiter) "^::.*")
  (run-hooks 'twee-mode-hook) ;;  turn on font-lock, abbrev-mode, etc
  )

(defun twee-insert-delimiters (begin end)
  "Insert a pair of delimiter strings and place point between them."
  (insert (concat begin end))
  (backward-char (length end)))

;; Command version.
(defun twee-delim-command (begin end)
  `(lambda () (interactive)
     (twee-insert-delimiters ,begin ,end)))

(defmacro twee-no-longlines (&rest body)
  "Do body while not in longlines-mode.  Switches back even if there's an error.  Motivation: so that inserting a newline (\"\\n\") would really insert a newline.  WARNING: switching modes invalidates info from the last match, so be sure to save (match-string) result if you need it.  See twee-add-item."
  `(unwind-protect
       (progn
         (longlines-mode nil)
         ,@body)
     (longlines-mode t)))

(defun twee-add-item ()
  "Add a numbered or bulletted item.  Repeats the most recent item marker."
  (interactive)
  ;; Copy initial sequence of asterisks or pound signs and final spaces.
  (save-excursion (re-search-backward "^\\(\\*+\\|#+\\) *"))
  (let ((s (match-string 0)))
    (twee-no-longlines
     (insert "\n" s)))
  )

(defun twee-add-sublist ()
  "Start a sub-list, with surrounding NestedSlider.  Only works in a list,
since it searches for the most recent list item marker."
  (interactive)
  (save-excursion (re-search-backward "^\\(\\*+\\|#+\\) *"))
  (let* ((match     (match-string 0))
         (match-len (length match))
         (spaces    (twee-spaces match-len)))
    (twee-no-longlines
     (insert "++++\n*" spaces "\n" spaces "=== \n" match)
     ;; Done inserting.  Move to first item in sublist
     (previous-line 2)
     (end-of-line))))

(defun twee-spaces (len)
  "Make a string of spaces with the given length."
  ;; Isn't there a simple, general, and efficient way to do this?
  (substring "                                                   "
             0 len))

(provide 'twee-mode)
