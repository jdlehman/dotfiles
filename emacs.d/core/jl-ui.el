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

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; syntax highlighting everywhere
(global-font-lock-mode 1)

;; Add proper word wrapping
(global-visual-line-mode t)

;; load zenburn theme
(load-theme 'zenburn t)

(provide 'jl-ui)
