(require 'visual-regexp)

;; replace all occurrences
(define-key global-map (kbd "C-c r") 'vr/replace)
;; ask for eaach occurrence
(define-key global-map (kbd "C-c q") 'vr/query-replace)
;; mark multiple-cursors based on regex
(define-key global-map (kbd "C-c m") 'vr/mc-mark)

(provide 'jl-visual-regexp)
