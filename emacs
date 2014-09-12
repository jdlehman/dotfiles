;; set up package managers
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages
  '(evil evil-leader)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; setup evil {{
(evil-mode 1) ;; evil mode
(global-evil-leader-mode) ;; evil leader

;; use evil mode in all emacs buffer states (except minibuffer)
(setq evil-motion-state-modes (append evil-emacs-state-modes evil-motion-state-modes))
(setq evil-emacs-state-modes nil)
;; }}

;; evil keybindings {{
(evil-leader/set-leader ",")
(evil-leader/set-key
  "v" 'split-window-horizontally
  "s" 'split-window-vertically)
(define-key evil-insert-state-map (kbd "jk") 'evil-normal-state)
(define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
;;}}
