;; use most recent files
(setq load-prefer-newer t)

;; set package archives
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
;; set package-user-dir to be relative to Prelude install path
(setq package-user-dir (expand-file-name "elpa" jl-dir))

(package-initialize)

;; update all out-of-date packages
(unless package-archive-contents
  (package-refresh-contents))

(defvar jl-language-packs
  '(clojure-mode
     coffee-mode
     css-mode
     markdown-mode
     scss-mode))

(defvar jl-utility-packs
  '(evil
     evil-leader
     flycheck
     magit
     zenburn-theme))

(defvar jl-packs
  (append jl-language-packs jl-utility-packs)
  "A list of packages to ensure are installed at launch.")

;; install each package if not already installed
(defun install-packages (packages)
  (dolist (p packages)
    (unless (package-installed-p p)
      (package-install p))))

(install-packages jl-packs)

(provide 'jl-packages)
