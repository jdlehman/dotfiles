(require 'guide-key)

;;; FUNCTIONS {{{
;;; }}}

;;; SETTINGS {{{
  (setq guide-key/guide-key-sequence '("C-x" "C-c")
        guide-key/recursive-key-sequence-flag t
        guide-key/popup-window-position 'right
        guide-key/idle-delay 0.5
        guide-key/text-scale-amount -1)

  (guide-key-mode 1)
;;; }}}

;;; KEYBINDINGS {{{
;;; }}}

;;; EVIL MAPPINGS {{{
;;; }}}

(provide 'jl-guide-key)
