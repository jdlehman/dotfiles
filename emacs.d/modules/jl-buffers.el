;;; FUNCTIONS {{{
  ;; from https://gist.github.com/mads379/3402786
  (defun jl/toggle-fullscreen-buffer ()
    "Make current buffer fullscreen"
    (interactive)
    (if (= 1 (length (window-list)))
      (jump-to-register '_)
      (progn
        (window-configuration-to-register '_)
        (delete-other-windows))))

  ;; from spacemacs
  (defun jl/last-buffer ()
    "Switch to last open buffer."
    (interactive)
    (switch-to-buffer (other-buffer (current-buffer) 1)))

  ;; from spacemacs
  (defun jl/new-empty-buffer ()
    "Create a new buffer called untitled(<n>)"
    (interactive)
    (let ((newbuf (generate-new-buffer-name "untitled")))
      (switch-to-buffer newbuf)))

  ;; found at http://emacswiki.org/emacs/KillingBuffers
  (defun jl/kill-other-buffers ()
    "Kill all other buffers."
    (interactive)
    (let (name (buffer-name))
      (when (yes-or-no-p (format "Killing all buffers except \"%s\" ? " buffer-file-name))
        (mapc 'kill-buffer (delq (current-buffer) (buffer-list)))
        (message "Buffers deleted!"))))

  ;; from magnars
  (defun jl/delete-current-buffer-file ()
    "Removes file connected to current buffer and kills buffer."
    (interactive)
    (let ((filename (buffer-file-name))
          (buffer (current-buffer))
          (name (buffer-name)))
      (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
        (when (yes-or-no-p "Are you sure you want to delete this file? ")
          (delete-file filename t)
          (kill-buffer buffer)
          (message "File '%s' successfully removed" filename)))))

  ;; from magnars
  (defun jl/rename-current-buffer-file ()
    "Renames current buffer and file it is visiting."
    (interactive)
    (let ((name (buffer-name))
          (filename (buffer-file-name)))
      (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
        (let ((new-name (read-file-name "New name: " filename)))
          (cond ((get-buffer new-name)
                 (error "A buffer named '%s' already exists!" new-name))
                (t
                  (rename-file filename new-name 1)
                  (rename-buffer new-name)
                  (set-visited-file-name new-name)
                  (set-buffer-modified-p nil)
                  (message "File '%s' successfully renamed to '%s'" name (file-name-nondirectory new-name))))))))
;;; }}}

;;; SETTINGS {{{
;;; }}}

;;; KEYBINDINGS {{{
  ;; toggle fullscreen buffer
  (global-set-key (kbd "C-c m") 'jl/toggle-fullscreen-buffer)
;;; }}}

;;; EVIL MAPPINGS {{{
;;; }}}

(provide 'jl-buffers)
