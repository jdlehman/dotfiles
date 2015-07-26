(require 'evil)
(require 'evil-leader)

;;; FUNCTIONS {{{
  ;; Save session including tabs
  ;; http://stackoverflow.com/questions/22445670/save-and-restore-elscreen-tabs-and-split-frames
  (defun jl/session-save ()
    "Store the elscreen tab configuration."
    (interactive)
    (if (desktop-save emacs-configuration-directory)
      (with-temp-file elscreen-tab-configuration-store-filename
                      (insert (prin1-to-string (elscreen-get-screen-to-name-alist))))))

  ;; Load session including tabs
  (defun jl/session-load ()
    "Restore the elscreen tab configuration."
    (interactive)
    (if (desktop-read)
      (let ((screens (reverse
                       (read
                         (with-temp-buffer
                           (insert-file-contents elscreen-tab-configuration-store-filename)
                           (buffer-string))))))
        (while screens
               (setq screen (car (car screens)))
               (setq buffers (split-string (cdr (car screens)) ":"))
               (if (eq screen 0)
                 (switch-to-buffer (car buffers))
                 (elscreen-find-and-goto-by-buffer (car buffers) t t))
               (while (cdr buffers)
                      (switch-to-buffer-other-window (car (cdr buffers)))
                      (setq buffers (cdr buffers)))
               (setq screens (cdr screens))))))

  ;; undefine evil keybinding
  ;; eg: (define-key evil-normal-state-map (kbd "TAB") 'jl/evil-undefine)
  ;; http://dnquark.com/blog/2012/02/emacs-evil-ecumenicalism/
  (defun jl/evil-undefine ()
    (interactive)
    (let (evil-mode-map-alist)
      (call-interactively (key-binding (this-command-keys)))))
;;; }}}

;;; SETTINGS {{{
  (evil-mode 1) ; evil mode
  (setq evil-leader/in-all-states 1)
  (global-evil-leader-mode) ; evil leader

  ;; use evil mode in all emacs buffer states (except minibuffer)
  (setq evil-motion-state-modes
        (append evil-emacs-state-modes evil-motion-state-modes)
        evil-emacs-state-modes nil)

  ;; do not move cursor back on return to normal mode
  ; (setq evil-move-cursor-back nil)

  ;; include last character in visual selection
  ;(setq evil-want-visual-char-semi-exclusive t)

  ;; visual cues for evil state
  (setq evil-emacs-state-cursor '("red" box))
  (setq evil-normal-state-cursor '("green" box))
  (setq evil-visual-state-cursor '("orange" box))
  (setq evil-insert-state-cursor '("red" bar))
  (setq evil-replace-state-cursor '("red" bar))
  (setq evil-operator-state-cursor '("red" hollow))

;;; }}}

;;; KEYBINDINGS {{{
;;; }}}

;;; EVIL MAPPINGS {{{
  (evil-leader/set-leader ",")

  ;; toggle between buffers
  (evil-leader/set-key "TAB" 'jl/last-buffer)

  ;; splits
  (evil-leader/set-key
    "v" 'split-window-horizontally
    "s" 'split-window-vertically)

  ;; a: applications
  (evil-leader/set-key
    "ac"  'calc-dispatch
    "ad"  'dired
    "ai"  'irc
    "ap"  'proced
    "ase" 'eshell
    "asi" 'shell
    "au"  'undo-tree-visualize)

  ;; b: buffers
  (evil-leader/set-key
    "bd"  'jl/delete-current-buffer-file
    "bK"  'jl/kill-other-buffers
    "bk"  'ido-kill-buffer
    "bn"  'switch-to-next-buffer
    "bN"  'jl/new-empty-buffer
    "bp"  'switch-to-prev-buffer
    "br"  'jl/rename-current-buffer-file
    "bw"  'toggle-read-only)

  ;; e: errors
  (evil-leader/set-key
    "en" 'next-error
    "ep" 'previous-error)

  ;; h: help
  (evil-leader/set-key
    "hf" 'describe-function
    "hk" 'describe-key
    "hm" 'describe-mode
    "hv" 'describe-variable)

  ;; remove evil insert keybindings (except ESC) and
  ;; use Emacs keybindings when in insert mode
  (setcdr evil-insert-state-map nil)
  (define-key evil-insert-state-map [escape] 'evil-normal-state)
  (define-key evil-insert-state-map
    (read-kbd-macro evil-toggle-key) 'evil-emacs-state)

  ;; Make C-g work like <esc>
  (define-key evil-normal-state-map "\C-g" 'evil-normal-state)
  (define-key evil-visual-state-map "\C-g" 'evil-normal-state)
  (define-key evil-insert-state-map "\C-g" 'evil-normal-state)

  (define-key evil-insert-state-map (kbd "jk") 'evil-normal-state)
  (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
  (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
  (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
  (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)

  ;; j and k work on wrapped lines
  (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

  ;;; other package keybindings {{{
    (define-key evil-normal-state-map (kbd "SPC") 'ace-jump-mode)
  ;;; }}}
;;; }}}

(provide 'jl-evil)
