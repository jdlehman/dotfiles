(require 'swiper)

;;; FUNCTIONS {{{
;;; }}}

;;; SETTINGS {{{
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
;;; }}}

;;; KEYBINDINGS {{{
  (global-set-key "\C-s" 'swiper)
  (global-set-key "\C-r" 'swiper)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key [f6] 'ivy-resume)
;;; }}}

;;; EVIL MAPPINGS {{{
;;; }}}

(provide 'jl-swiper)
