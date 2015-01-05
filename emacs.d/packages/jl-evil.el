(require 'evil)
(require 'evil-leader)

;;; FUNCTIONS {{{
;;; }}}

;;; SETTINGS {{{
  (evil-mode 1) ; evil mode
  (global-evil-leader-mode) ; evil leader

  ;; use evil mode in all emacs buffer states (except minibuffer)
  (setq evil-motion-state-modes
        (append evil-emacs-state-modes evil-motion-state-modes)
        evil-emacs-state-modes nil)
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

  ;; Make C-g work like <esc>
  (define-key evil-normal-state-map "\C-g" 'evil-normal-state)
  (define-key evil-visual-state-map "\C-g" 'evil-normal-state)
  (define-key evil-insert-state-map "\C-g" 'evil-normal-state)

  ;; use Emacs keybindings when in insert mode
  ; (setcdr evil-insert-state-map nil)
  ; (define-key evil-insert-state-map [escape] 'evil-normal-state)

  (define-key evil-insert-state-map (kbd "jk") 'evil-normal-state)
  (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
  (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
  (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
  (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
  ;;; other package keybindings {{{
    (define-key evil-normal-state-map (kbd "SPC") 'ace-jump-mode)
  ;;; }}}
;;; }}}

(provide 'jl-evil)
