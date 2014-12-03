;; setup backups
(setq
   ;; don't clobber symlinks
   backup-by-copying t
   ;; store backups in centralized folder
   backup-directory-alist `(("." . ,jl-backups-dir))
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   ;; use versioned backups
   version-control t)

;; show trailing whitespace
(setq show-trailing-whitespace t
      whitespace-style '(face trailing))

;; save cursor position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" jl-dir))

;; set modifier bindings
(setq ns-command-modifier 'meta
      ns-option-modifier 'super
      ns-function-modifier 'hyper)

(provide 'jl-base)
