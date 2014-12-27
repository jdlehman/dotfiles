(require 'ace-jump-mode)
(require 'ace-jump-buffer)
(require 'ace-window)

(global-set-key (kbd "C-c SPC") 'ace-jump-mode)
(global-set-key (kbd "C-c w") 'ace-window)

;; set ace window keys to be home keys
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

(provide 'jl-ace-jump)
