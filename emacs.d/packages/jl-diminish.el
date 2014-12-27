(require 'diminish)

;;; FUNCTIONS {{{
  ;; from https://github.com/hrs/dotfiles/blob/master/emacs.d/lisp/hide-modes.el
  (defmacro jl/diminish-minor-mode (package-name mode &optional abbrev)
    `(eval-after-load (symbol-name ,package-name)
                      '(diminish ,mode ,abbrev)))

  (defmacro jl/diminish-major-mode (mode-hook abbrev)
    `(add-hook ,mode-hook
               (lambda () (setq mode-name ,abbrev))))
;;; }}}

;; rename
(jl/diminish-minor-mode 'undo-tree 'undo-tree-mode " UT")
(jl/diminish-major-mode 'emacs-lisp-mode-hook "el")
(jl/diminish-major-mode 'js2-mode-hook "js2")
(jl/diminish-major-mode 'clojure-mode-hook "clj")

;; hide
(jl/diminish-minor-mode 'magit 'magit-auto-revert-mode)
(jl/diminish-minor-mode 'guide-key 'guide-key-mode)
(jl/diminish-minor-mode 'whitespace 'whitespace-mode)
(jl/diminish-minor-mode 'yasnippet 'yas-minor-mode)
(jl/diminish-minor-mode 'eldoc 'eldoc-mode)
(jl/diminish-minor-mode 'helm 'helm-mode)

(provide 'jl-diminish)
