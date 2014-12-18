(require 'ido)
(require 'flx-ido)
(require 'ido-ubiquitous)

(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-max-prospects 10
      ido-default-file-method 'selected-window)

;; disable ido faces to see flx highlights.
(setq ido-use-faces nil)

(ido-mode 1)
;; ido everywhere
(ido-ubiquitous-mode 1)
;; fuzzy matching for ido
(flx-ido-mode 1)

(provide 'jl-ido)
