(require 'web-mode)

(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))

(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)

(setq web-mode-script-padding 2)
(setq web-mode-style-padding 2)

(setq web-mode-enable-auto-pairing t)
(setq web-mode-enable-current-element-highlight t)

(provide 'jl-web-mode)
