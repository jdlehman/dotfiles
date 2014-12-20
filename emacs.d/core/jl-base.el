;; store backups in temp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))

;; store auto-saves in temp dir
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      auto-save-list-file-prefix (concat temporary-file-directory ".auto-saves-"))

;; do not create lockfiles (.#file-name)
(setq create-lockfiles nil)

;; create new lines on movement at EOF
(setq next-line-add-newlines t)

;; smooth scrolling
(setq scroll-conservatively 10
      scroll-margin 7)


;; show trailing whitespace
(setq show-trailing-whitespace t
      whitespace-style '(face trailing))

;; save cursor position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" jl/root-dir))

;; set modifier bindings
(setq ns-command-modifier 'meta
      ns-option-modifier 'super
      ns-function-modifier 'hyper)

;; instantly display current keystrokes in mini buffer
(setq echo-keystrokes 0.1)

;; auto-indent on RET
(define-key global-map (kbd "RET") 'newline-and-indent)

(provide 'jl-base)
