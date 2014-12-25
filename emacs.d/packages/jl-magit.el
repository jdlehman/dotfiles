;; point to correct version of emacsclient so magit doesnt error
(setq magit-emacsclient-executable nil)

(require 'magit)

;;; FUNCTIONS {{{
;;; }}}

;;; SETTINGS {{{
;;; }}}

;;; KEYBINDINGS {{{
  (global-set-key (kbd "C-c g s") 'magit-status)
;;; }}}

(provide 'jl-magit)
