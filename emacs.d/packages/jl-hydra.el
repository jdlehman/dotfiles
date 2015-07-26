(require 'hydra)

;;; FUNCTIONS {{{
(defhydra hydra-zoom (jl/command-map "z")
  "zoom"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out"))
;;; }}}

;;; SETTINGS {{{
  (setq hydra-is-helpful t)
;;; }}}

;;; KEYBINDINGS {{{
;;; }}}

;;; EVIL MAPPINGS {{{
;;; }}}

(provide 'jl-hydra)
