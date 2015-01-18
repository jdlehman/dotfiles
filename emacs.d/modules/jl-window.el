;;; FUNCTIONS {{{
  ;; window resize functions from spacemacs
  (defun jl/resize-window-doc ()
    (jl/echo (format
               "[%sx%s] Resize window: (H/L) shrink/enlarge horizontally, (J/K) shrink/enlarge vertically"
               (window-total-width) (window-total-height))))

  (defun jl/resize-window-overlay-map ()
    "Set a temporary overlay map to easily resize a window."
    (interactive)
    (set-transient-map
      (let ((window-resize-map (make-sparse-keymap)))
        (define-key window-resize-map (kbd "H") 'jl/shrink-window-horizontally)
        (define-key window-resize-map (kbd "J") 'jl/shrink-window)
        (define-key window-resize-map (kbd "K") 'jl/enlarge-window)
        (define-key window-resize-map (kbd "L") 'jl/enlarge-window-horizontally)
        window-resize-map) t)
    (jl/resize-window-doc))

  (defun jl/shrink-window-horizontally (delta)
    "Wrap `jl/shrink-window-horizontally'."
    (interactive "p")
    (shrink-window delta t)
    (jl/resize-window-overlay-map))

  (defun jl/shrink-window (delta)
    "Wrap `jl/shrink-window'."
    (interactive "p")
    (shrink-window delta)
    (jl/resize-window-overlay-map))

  (defun jl/enlarge-window (delta)
    "Wrap `jl/enlarge-window'."
    (interactive "p")
    (enlarge-window delta)
    (jl/resize-window-overlay-map))

  (defun jl/enlarge-window-horizontally (delta)
    "Wrap `jl/enlarge-window-horizontally'."
    (interactive "p")
    (enlarge-window delta t)
    (jl/resize-window-overlay-map))
;;; }}}

;;; SETTINGS {{{
;;; }}}

;;; KEYBINDINGS {{{
  (global-set-key (kbd "C-c w s") 'jl/resize-window-overlay-map)
;;; }}}

;;; EVIL MAPPINGS {{{
;;; }}}

(provide 'jl-window)
