(require 'ido)
(require 'flx-ido)
(require 'ido-ubiquitous)

(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-max-prospects 10
      ido-save-directory-list-file (expand-file-name "ido.hist" jl/history-dir)
      ido-default-file-method 'selected-window
      ido-auto-merge-work-directories-length -1)

;; disable ido faces to see flx highlights.
(setq ido-use-faces nil)

(ido-mode 1)
;; ido everywhere
(ido-ubiquitous-mode 1)
;; fuzzy matching for ido
(flx-ido-mode 1)

;; remember recently and frequently used commands
(require 'smex)

(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

(setq smex-save-file (expand-file-name ".smex-items" jl/history-dir)
      smex-show-unbound-commands t)

(provide 'jl-ido)
