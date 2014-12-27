;;; FUNCTIONS {{{
  ;; from http://vickychijwani.me/nuggets-from-my-emacs-part-i/
  (defun jl/comment-or-uncomment-region-or-line ()
    "Like comment-or-uncomment-region, but if there's no mark \(that means no
    region\) apply comment-or-uncomment to the current line"
    (interactive)
    (if (not mark-active)
      (comment-or-uncomment-region
        (line-beginning-position) (line-end-position))
      (if (< (point) (mark))
        (comment-or-uncomment-region (point) (mark))
        (comment-or-uncomment-region (mark) (point)))))
;;; }}}

;;; SETTINGS {{{
;;; }}}

;;; KEYBINDINGS {{{
  (global-set-key (kbd "C-c C-r") 'jl/comment-or-uncomment-region-or-line)
;;; }}}

(provide 'jl-comment)
