(defvar jl/root-dir (file-name-directory load-file-name)
  "root dir of this emacs distribution")

(defvar jl/core-dir (expand-file-name "core" jl/root-dir)
  "dir containing core functionality")

(defvar jl/modules-dir (expand-file-name "modules" jl/root-dir)
  "dir containing modules")

(defvar jl/backups-dir (expand-file-name "backups" jl/root-dir)
  "dir containing emacs backups/autosaves")

(defvar jl/history-dir (expand-file-name "history" jl/root-dir)
  "dir containing history files")

(defvar jl/modules (expand-file-name "jl-modules.el" jl/core-dir)
  "file contains a list of modules to be required.")

;; create backups dir if it does not already exist
(unless (file-exists-p jl/backups-dir)
  (make-directory jl/backups-dir))

;; create history dir if it does not already exist
(unless (file-exists-p jl/history-dir)
  (make-directory jl/history-dir))

;; add directories to emacs's load-path
(add-to-list 'load-path jl/core-dir)
(add-to-list 'load-path jl/modules-dir)

;; Package Manager
;; See Cask file for its configuration
;; https://github.com/cask/cask
(defvar cask-location (car (file-expand-wildcards "/usr/local/Cellar/cask/*/cask.el")))
(require 'cask cask-location)
(cask-initialize)

;; Keeps Cask file in sync with the packages
;; that you install/uninstall via ~M-x list-packages~
;; https://github.com/rdallasgray/pallet
(require 'pallet)

;; require core
(require 'jl-ui)
(require 'jl-base)

;; load modules
(load jl/modules)
