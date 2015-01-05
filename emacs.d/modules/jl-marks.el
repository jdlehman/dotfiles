;; push-mark-no-activate and jump-to-mark from:
;; http://www.masteringemacs.org/article/fixing-mark-commands-transient-mark-mode

;;; FUNCTIONS {{{
  (defun jl/push-mark-no-activate ()
    "Pushes `point' to `mark-ring' and does not activate the region
    Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
    (interactive)
    (push-mark (point) t nil)
    (message "Pushed mark to ring"))

  (defun jl/jump-to-mark ()
    "Jumps to the local mark, respecting the `mark-ring' order.
    This is the same as using \\[set-mark-command] with the prefix argument."
    (interactive)
    (set-mark-command 1))

;;; }}}

;;; SETTINGS {{{
;;; }}}

;;; KEYBINDINGS {{{
  ;; add mark without activating region
  (global-set-key (kbd "C-`") 'jl/push-mark-no-activate)

  ;; jump to mark, without activating region
  (global-set-key (kbd "M-`") 'jl/jump-to-mark)
;;; }}}

;;; EVIL MAPPINGS {{{
;;; }}}

(provide 'jl-marks)
