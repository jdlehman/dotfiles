(require 'helm-config)
(require 'helm-ag)
(require 'helm-projectile)
(require 'helm-descbinds)

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

  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t))

  (helm-mode 1)
  (helm-descbinds-mode)
  ;; use helm with projectile
  (helm-projectile-on)
  (setq projectile-completion-system 'helm)
;;; }}}

;;; KEYBINDINGS {{{
  ;; set helm command prefix
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))

  ;; add additional helm commands
  (define-key helm-command-map (kbd "o") 'helm-occur)
  (define-key helm-command-map (kbd "a") 'helm-ag)
  (define-key helm-command-map (kbd "d a") 'helm-do-ag)
  (define-key helm-command-map (kbd "g") 'helm-google-suggest)
  (define-key helm-command-map (kbd "w") 'helm-wikipedia-suggest)
  (define-key helm-command-map (kbd "SPC") 'helm-all-mark-rings)

  ;; from prelude's helm-everywhere:
  ;; override existing commands with helm ones
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)
  (global-set-key (kbd "C-x b") 'helm-mini)
  (global-set-key (kbd "C-x C-b") 'helm-buffers-list)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "C-h f") 'helm-apropos)
  (global-set-key (kbd "C-h r") 'helm-info-emacs)
  (global-set-key (kbd "C-h C-l") 'helm-locate-library)
  (global-set-key (kbd "C-c f") 'helm-recentf)
;;; }}}

;;; EVIL MAPPINGS {{{
;;; }}}

(provide 'jl-helm)
