;;; keybinding helper functions
(defun core/reload-emacs-config ()
  (interactive)
  (load-file "~/.emacs.d/init.el"))

(defun core/edit-emacs-config ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun core/my-jk ()
  (interactive)
  (if (eq major-mode 'vterm-mode)
      (vterm--self-insert)
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
        (insert initial-key)))))

(defun core/change-window-below ()
  (evil-window-split))

(defun core/change-window-right ()
  (evil-window-vsplit))

;; counsel-rg
(defun core/counsel-rg-split (occur)
  "Go to an occurrence in a horizontal split."
  (interactive)
  (core/change-window-below)
  (counsel-git-grep-action occur))

(defun core/counsel-rg-vsplit (occur)
  "Go to an occurrence in a vertical split."
  (interactive)
  (core/change-window-right)
  (counsel-git-grep-action occur))

;; counsel-fzf
(defun core/counsel-fzf (&optional input)
  "Run `fzf' in tandem with `fd' to find files."
  (interactive)
  (let ((process-environment
         (cons (concat "FZF_DEFAULT_COMMAND=fd -IHLE .git -t f")
               process-environment)))
    (counsel-fzf input)))

(defun core/counsel-fzf-split (file)
  "Open a file with fzf in a horizontal split."
  (interactive)
  (core/change-window-below)
  (find-file (concat (projectile-project-root) file)))

(defun core/counsel-fzf-vsplit (file)
  "Open a file with fzf in a vertical split."
  (interactive)
  (core/change-window-right)
  (find-file (concat (projectile-project-root) file)))

;; counsel-find-file
(defun core/counsel-find-file-split (file)
  "Open a file in a horizontal split."
  (interactive)
  (core/change-window-below)
  (counsel-find-file-action file))

(defun core/counsel-find-file-vsplit (file)
  "Open a file in a vertical split."
  (interactive)
  (core/change-window-right)
  (counsel-find-file-action file))

;; counsel-switch-buffer
(defun core/counsel-switch-buffer-split (buffer)
  "Open a buffer in a horizontal split."
  (interactive)
  (core/change-window-below)
  (ivy--switch-buffer-action buffer))

(defun core/counsel-switch-buffer-vsplit (buffer)
  "Open a buffer in a vertical split."
  (interactive)
  (core/change-window-right)
  (ivy--switch-buffer-action buffer))

;; projectile-switch-project
(defun core/projectile-switch-project-split (project)
  "Open a project in a horizontal split."
  (interactive)
  (core/change-window-below)
  (projectile-switch-project-by-name project))

(defun core/projectile-switch-project-vsplit (project)
  "Open a project in a vertical split."
  (interactive)
  (core/change-window-right)
  (projectile-switch-project-by-name project))

;; vterm
(defun core/vterm-split ()
  "Open vterm in a horizontal split."
  (interactive)
  (core/change-window-below)
  (multi-vterm))

(defun core/vterm-vsplit ()
  "Open vterm in a vertical split."
  (interactive)
  (core/change-window-right)
  (multi-vterm))
