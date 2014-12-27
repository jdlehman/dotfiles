(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

;; store backups in temp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))

;; store auto-saves in temp dir
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      auto-save-list-file-prefix (concat temporary-file-directory ".auto-saves-"))

;; do not create lockfiles (.#file-name)
(setq create-lockfiles nil)

;; create new lines on movement at EOF
(setq next-line-add-newlines t)

;; smooth scrolling
(setq scroll-conservatively 10
      scroll-margin 7)

;; savehist keeps track of some history
(require 'savehist)
(setq savehist-additional-variables
      ;; search entries
      '(search-ring regexp-search-ring)
      ;; save every minute
      savehist-autosave-interval 60
      savehist-file (expand-file-name "savehist" jl/history-dir))
(savehist-mode 1)

;; save recent files
(require 'recentf)
(setq recentf-save-file (expand-file-name "recentf" jl/history-dir)
      recentf-max-saved-items 100
      recentf-max-menu-items 15)

;; save bookmarks
(require 'bookmark)
(setq bookmark-default-file (expand-file-name "bookmarks" jl/history-dir)
      bookmark-save-flag 1)

;; save cursor position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" jl/history-dir))

;; set modifier bindings
(setq ns-command-modifier 'meta
      ns-option-modifier 'super
      ns-function-modifier 'hyper)

;; instantly display current keystrokes in mini buffer
(setq echo-keystrokes 0.1)

;; delete the selection area with a keypress
; (delete-selection-mode t)

;; visible bell
(setq visible-bell t)

;; from magnars
;; Keep region when undoing in region
(defadvice undo-tree-undo (around keep-region activate)
           (if (use-region-p)
             (let ((m (set-marker (make-marker) (mark)))
                   (p (set-marker (make-marker) (point))))
               ad-do-it
               (goto-char p)
               (set-mark m)
               (set-marker p nil)
               (set-marker m nil))
             ad-do-it))

;;; keybindings {{{
  ;; auto-indent on RET
  (global-set-key (kbd "RET") 'newline-and-indent)
  ;; remove ESC keybindings (use Meta instead). now equivalent to C-g
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit)
;;; }}}

(provide 'jl-base)
