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

  ;; http://endlessparentheses.com/implementing-comment-line.html
  (defun jl/comment-line (n)
    "Comment or uncomment current line and leave point after it.
    With positive prefix, apply to N lines including current one.
    With negative prefix, apply to -N lines above."
    (interactive "p")
    (comment-or-uncomment-region
      (line-beginning-position)
      (goto-char (line-end-position n)))
    (forward-line 1)
    (back-to-indentation))
;;; }}}

;;; SETTINGS {{{
;;; }}}

;;; KEYBINDINGS {{{
  (global-set-key (kbd "C-c C-r") #'jl/comment-or-uncomment-region-or-line)
  (global-set-key (kbd "C-;") #'jl/comment-line)
;;; }}}

;;; EVIL MAPPINGS {{{
;;; }}}

(provide 'jl-comment)
