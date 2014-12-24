;; point to correct version of emacsclient so magit doesnt error
(setq magit-emacsclient-executable nil)

(require 'magit)

;;; FUNCTIONS {{{
;;; }}}

;;; SETTINGS {{{
;;; }}}

;;; KEYBINDINGS {{{
  (define-key global-map (kbd "C-c g s") 'magit-status)
;;; }}}

(provide 'jl-magit)
