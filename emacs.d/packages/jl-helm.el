(require 'helm-config)

;;; FUNCTIONS {{{
;;; }}}

;;; SETTINGS {{{
  (setq
    ;; open helm buffer inside current window, not occupy whole other window
    helm-split-window-in-side-p t
    ;; move to end or beginning of source when reaching top or bottom of source.
    helm-move-to-line-cycle-in-source t
    ;; search for library in `require' and `declare-function' sexp.
    helm-ff-search-library-in-sexp t
    ;; scroll 8 lines other window using M-<next>/M-<prior>
    helm-scroll-amount 8
    helm-ff-file-name-history-use-recentf t)

  (helm-mode 1)
;;; }}}

;;; KEYBINDINGS {{{
  ;; set helm command prefix
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))
;;; }}}

(provide 'jl-helm)
