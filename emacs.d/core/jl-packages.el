(require 'package)

;; use most recent files
(setq load-prefer-newer t)

;; set package-user-dir
(setq package-user-dir (expand-file-name "elpa" jl-dir))

;; set package archives
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))

(package-initialize)

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

;; install/update each package if not already installed
(defun install-packages ()
  (interactive)
  (package-refresh-contents)
  (dolist (package jl-packs)
    (unless (package-installed-p package)
      (package-install package))))

(install-packages)

(provide 'jl-packages)
