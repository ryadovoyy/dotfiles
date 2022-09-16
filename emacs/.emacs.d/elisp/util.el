;; keybinding helper functions
(defun core/reload-emacs-config ()
  (interactive)
  (load-file "~/.emacs.d/init.el"))

(defun core/edit-emacs-config ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun core/my-jk ()
  (interactive)
  (let* ((initial-key ?j)
         (final-key ?k)
         (timeout 0.5)
         (event (read-event nil nil timeout)))
    (if event
        ;; timeout met
        (if (and (characterp event) (= event final-key))
            (evil-normal-state)
          (insert initial-key)
          (push event unread-command-events))
      ;; timeout exceeded
      (insert initial-key))))

(defun core/counsel-fzf (&optional input)
  "Run `fzf' in tandem with `fd' to find files."
  (interactive)
  (let ((process-environment
         (cons (concat "FZF_DEFAULT_COMMAND=fd -IHLE .git -t f")
               process-environment)))
    (counsel-fzf input)))

(defun core/other-window (filename)
  "Open a file in another window."
  (interactive)
  (split-window-right)
  (other-window 1)
  (find-file (concat (projectile-project-root) filename)))
