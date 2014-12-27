;; disable menu/toolbar/scrollbar
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

;; open emacs blank scratch
(setq inhibit-splash-screen t
      initial-scratch-message nil)

;; status bar settings
(column-number-mode t)

;; display and format line numbers
(global-linum-mode t)
(setq linum-format "%2d ")

;; highlight current line
(global-hl-line-mode 1)
(set-face-background 'hl-line "#330")

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; show trailing whitespace
(require 'whitespace)
(setq whitespace-line-column 80
      whitespace-style '(face lines-tail trailing))
(add-hook 'prog-mode-hook 'whitespace-mode)

;; syntax highlighting everywhere
(global-font-lock-mode 1)

;; Add proper word wrapping
(global-visual-line-mode t)

;; set tabs to 2 spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq indent-line-function 'insert-tab)

;;; rename modes on modeline {{{
  (rename-modeline "js2-mode" js2-mode "JS2")
  (rename-modeline "clojure-mode" clojure-mode "Clj")
;;; }}}

;; load zenburn theme
(load-theme 'zenburn t)

(provide 'jl-ui)
