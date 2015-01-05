(require 'ace-jump-mode)
(require 'ace-jump-buffer)
(require 'ace-window)

;;; FUNCTIONS {{{
;;; }}}

;;; SETTINGS {{{
  ;; set ace window keys to be home keys
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
;;; }}}

;;; KEYBINDINGS {{{
  (global-set-key (kbd "C-c SPC") 'ace-jump-mode)
  (global-set-key (kbd "C-c w") 'ace-window)
;;; }}}

;;; EVIL MAPPINGS {{{
;;; }}}

(provide 'jl-ace-jump)
