(require 'ace-jump-mode)
(require 'ace-jump-buffer)

(global-set-key (kbd "C-c SPC") 'ace-jump-mode)
(global-set-key (kbd "C-x b") 'ace-jump-buffer)

(provide 'jl-ace-jump)
