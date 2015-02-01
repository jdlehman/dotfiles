(require 'dired)
(require 'dired-x)

;;; FUNCTIONS {{{
  ;; http://www.emacswiki.org/emacs/DiredReuseDirectoryBuffer

  ;; we want dired not not make always a new buffer if visiting a directory
  ;; but using only one dired buffer for all directories.
  (defadvice dired-advertised-find-file (around dired-subst-directory activate)
     "Replace current buffer if file is a directory."
     (interactive)
     (let ((orig (current-buffer))
           (filename (dired-get-filename)))
       ad-do-it
       (when (and (file-directory-p filename)
                  (not (eq (current-buffer) orig)))
         (kill-buffer orig))))
;;; }}}

;;; SETTINGS {{{
  (when (eq system-type 'darwin)
    (setq insert-directory-program "/usr/local/bin/gls"))
  (setq dired-listing-switches "-aBgFGh1v
                                --color
                                --group-directories-first")
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)

  (eval-after-load "dired"
    ;; don't remove `other-window', the caller expects it to be there
    '(defun dired-up-directory (&optional other-window)
       "Run Dired on parent directory of current directory."
       (interactive "P")
       (let* ((dir (dired-current-directory))
              (orig (current-buffer))
              (up (file-name-directory (directory-file-name dir))))
         (or (dired-goto-file (directory-file-name dir))
             ;; Only try dired-goto-subdir if buffer has more than one dir.
             (and (cdr dired-subdir-alist)
                  (dired-goto-subdir up))
             (progn
               (kill-buffer orig)
               (dired up)
               (dired-goto-file dir))))))
;;; }}}

;;; KEYBINDINGS {{{
;;; }}}

;;; EVIL MAPPINGS {{{
;;; }}}

(provide 'jl-dired)
