;;; FUNCTIONS {{{
  ;; from http://vickychijwani.me/nuggets-from-my-emacs-part-i/
  ;; "smart" home, i.e., home toggles b/w 1st non-blank character and 1st column
  (defun jl/smart-beginning-of-line ()
    "Move point to first non-whitespace character or beginning-of-line."
    (interactive "^") ; Use (interactive "^") in Emacs 23 to make shift-select work
    (let ((oldpos (point)))
      (back-to-indentation)
      (and (= oldpos (point))
           (beginning-of-line))))
;;; }}}

;;; SETTINGS {{{
;;; }}}

;;; KEYBINDINGS {{{
  ;; smart beginning of line
  (global-set-key [home] 'jl/smart-beginning-of-line)
  (global-set-key (kbd "C-a") 'jl/smart-beginning-of-line)

  ;; go to line
  (global-set-key (kbd "C-x l") 'goto-line)
;;; }}}

;;; EVIL MAPPINGS {{{
;;; }}}

(provide 'jl-movement)
