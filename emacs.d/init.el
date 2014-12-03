(defvar jl-dir (file-name-directory load-file-name)
  "root dir of this emacs distribution")

(defvar jl-core-dir (expand-file-name "core" jl-dir)
    "dir containing core functionality")

(defvar jl-modules-dir (expand-file-name "modules" jl-dir)
    "dir containing modules")

(defvar jl-backups-dir (expand-file-name "backups" jl-dir)
    "dir containing emacs backups/autosaves")

(defvar jl-modules (expand-file-name "jl-modules.el" jl-core-dir)
  "file contains a list of modules to be required.")

;; create backups dir if it does not already exist
(unless (file-exists-p jl-backups-dir)
  (make-directory jl-backups-dir))

;; add directories to emacs's load-path
(add-to-list 'load-path jl-core-dir)
(add-to-list 'load-path jl-modules-dir)

;; require core
(require 'jl-packages)
(require 'jl-ui)
(require 'jl-base)

;; load modules
(load jl-modules)
