;; Where things go when there is no place for them
;; If a pattern or related things appear, they will be moved into
;; a module of their own

;;; FUNCTIONS {{{
  ;; occur-do what i mean
  ;; http://oremacs.com/2015/01/26/occur-dwim/
  (defun occur-dwim ()
    "Call `occur' with a sane default."
    (interactive)
    (push (if (region-active-p)
            (buffer-substring-no-properties
              (region-beginning)
              (region-end))
            (thing-at-point 'symbol))
          regexp-history)
    (call-interactively 'occur))
;;; }}}

;;; SETTINGS {{{
;;; }}}

;;; KEYBINDINGS {{{
  (global-set-key (kbd "M-s o") #'occur-dwim)
;;; }}}

;;; EVIL MAPPINGS {{{
;;; }}}

(provide 'jl-misc)
