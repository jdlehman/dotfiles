;; inspiration from: http://endlessparentheses.com/the-toggle-map-and-wizardry.html

;; COMMAND-MAP
;;; prefix map for running custom commands {{{
  (define-prefix-command 'jl/command-map)
  (define-key ctl-x-map "j" 'jl/command-map)
;;; }}}

;; TOGGLE-MAP
;;; prefix map for toggling modes, variables, etc {{{
  (define-prefix-command 'jl/toggle-map)
  (define-key ctl-x-map "t" 'jl/toggle-map)

  (define-key jl/toggle-map "w" #'whitespace-mode)
;;; }}}

(provide 'jl-mode)
