;;; FUNCTIONS {{{
  ;; from http://endlessparentheses.com/ispell-and-abbrev-the-perfect-auto-correct.html
  (defun jl/ispell-word-then-abbrev (p)
    "Call `ispell-word'. Then create an abbrev for the correction made.
    With prefix P, create local abbrev. Otherwise it will be global."
    (interactive "P")
    (let ((bef (downcase (or (thing-at-point 'word) ""))) aft)
      (call-interactively 'ispell-word)
      (setq aft (downcase (or (thing-at-point 'word) "")))
      (unless (string= aft bef)
        (message "\"%s\" now expands to \"%s\" %sally"
                 bef aft (if p "loc" "glob"))
        (define-abbrev
          (if p local-abbrev-table global-abbrev-table)
          bef aft))))
;;; }}}

;;; SETTINGS {{{
  (setq save-abbrevs t)
  (setq-default abbrev-mode t)
;;; }}}

;;; KEYBINDINGS {{{
  (define-key ctl-x-map (kbd "C-i") 'jl/ispell-word-then-abbrev)
;;; }}}

;;; EVIL MAPPINGS {{{
;;; }}}

(provide 'jl-spelling)
