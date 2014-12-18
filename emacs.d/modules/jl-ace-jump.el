(require 'ace-jump-mode)
(require 'ace-jump-buffer)

(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
(define-key global-map (kbd "C-x b") 'ace-jump-buffer)

(provide 'jl-ace-jump)
