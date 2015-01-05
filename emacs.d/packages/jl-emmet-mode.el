(require 'emmet-mode)


;;; FUNCTIONS {{{
;;; }}}

;;; SETTINGS {{{
  ;; Auto-start on any markup modes
  (add-hook 'sgml-mode-hook 'emmet-mode)
  ;; enable Emmet's css abbreviation.
  (add-hook 'css-mode-hook  'emmet-mode)
;;; }}}

;;; KEYBINDINGS {{{
;;; }}}

;;; EVIL MAPPINGS {{{
;;; }}}

(provide 'jl-emmet-mode)
