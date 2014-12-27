(defvar jl/root-dir (file-name-directory load-file-name)
  "root dir of this emacs distribution")

(defvar jl/core-dir (expand-file-name "core" jl/root-dir)
  "dir containing core functionality")

(defvar jl/modules-dir (expand-file-name "modules" jl/root-dir)
  "dir containing modules")

(defvar jl/packages-dir (expand-file-name "packages" jl/root-dir)
  "dir containing package configs")

(defvar jl/history-dir (expand-file-name "history" jl/root-dir)
  "dir containing history files")

(defvar jl/packages (expand-file-name "jl-packages.el" jl/core-dir)
  "file contains a list of packages to be required.")

;; create history dir if it does not already exist
(unless (file-exists-p jl/history-dir)
  (make-directory jl/history-dir))

;; add directories to emacs's load-path
(add-to-list 'load-path jl/core-dir)
(add-to-list 'load-path jl/packages-dir)
(add-to-list 'load-path jl/modules-dir)

;;; Package Manager {{{
  ;; See Cask file for its configuration
  ;; https://github.com/cask/cask
  (defvar cask-location (car (file-expand-wildcards "/usr/local/Cellar/cask/*/cask.el")))
  (require 'cask cask-location)
  (cask-initialize)

  ;; Keeps Cask file in sync with the packages
  ;; that you install/uninstall via ~M-x list-packages~
  ;; https://github.com/rdallasgray/pallet
  (require 'pallet)
;;; }}}

;; require core
(require 'jl-util)
(require 'jl-ui)
(require 'jl-base)

;; load package configs
(load jl/packages)

;; load modules
(require 'jl-buffers)
(require 'jl-marks)
(require 'jl-movement)
(require 'jl-spelling)
(require 'jl-comment)
