(require 'projectile)

(projectile-global-mode)
(setq projectile-cache-file (expand-file-name  "projectile.cache" jl/history-dir))

(provide 'jl-projectile)
