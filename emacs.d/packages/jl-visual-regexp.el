(require 'visual-regexp)

;; replace all occurrences
(define-key global-map (kbd "C-c v r") 'vr/replace)
;; ask for eaach occurrence
(define-key global-map (kbd "C-c v q") 'vr/query-replace)
;; mark multiple-cursors based on regex
(define-key global-map (kbd "C-c v m") 'vr/mc-mark)

(provide 'jl-visual-regexp)
