(require 'projectile)

;;; FUNCTIONS {{{
  (defun jl/projectile-helm-ag ()
    (interactive)
    (helm-ag (projectile-project-root)))
;;; }}}

;;; SETTINGS {{{
  (projectile-global-mode)
  (setq projectile-cache-file (expand-file-name  "projectile.cache" jl/history-dir))
;;; }}}

;;; KEYBINDINGS {{{
  (define-key projectile-command-map (kbd "s a") 'jl/projectile-helm-ag)
;;; }}}

;;; EVIL MAPPINGS {{{
;;; }}}

(provide 'jl-projectile)
