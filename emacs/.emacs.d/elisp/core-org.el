(defun core/org-setup ()
  (org-indent-mode)
  (visual-line-mode))

(defun core/org-font-setup ()
  ;; replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
    '(("^ *\\([-]\\) " (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) ""))))))

  ;; set faces for heading levels
  (dolist (face '((org-level-1 . 1.3)
                  (org-level-2 . 1.2)
                  (org-level-3 . 1.1)
                  (org-level-4 . 1.05)
                  (org-level-5 . 1.0)
                  (org-level-6 . 1.0)))
    (set-face-attribute (car face) nil
      :font "JetBrainsMonoNL Nerd Font"
      :weight 'bold
      :height (cdr face))))

;; <built-in>
(use-package org
  :hook (org-mode . core/org-setup)
  :config
  (setq org-ellipsis " "
        org-hide-emphasis-markers t
        org-pretty-entities t
        org-directory "~/Documents/org/"
        org-agenda-files '("~/Documents/org/tags.org"
                           "~/Documents/org/tasks.org"
                           "~/Documents/org/birthdays.org")
        org-refile-targets '(("archive.org" :maxlevel . 1)
                             ("tasks.org" :maxlevel . 1))
        org-capture-templates
        '(("t" "Task" entry (file+headline "~/Documents/org/tasks.org" "Inbox")
           "* TODO %?\n%U\n%a" :empty-lines 1 :kill-buffer t)
          ("b" "Birthday" entry (file+headline "~/Documents/org/birthdays.org" "People")
           "* %^{Name}\n%^{Birthday}t" :empty-lines-before 1 :kill-buffer t))
        ;; agenda logging
        org-agenda-start-with-log-mode t
        org-log-done 'time
        org-log-into-drawer t)

  ;; save org buffers after refiling
  (advice-add 'org-refile :after #'org-save-all-org-buffers)
  (core/org-font-setup))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("" "" "" "" "" "")))

;; reader mode
(use-package visual-fill-column
  :hook (org-mode . visual-fill-column-mode)
  :custom
  (visual-fill-column-width 100)
  (visual-fill-column-center-text t)
  (visual-fill-column-enable-sensible-window-split t))

;; agenda notifications
(use-package org-alert
  :custom
  (alert-default-style 'notifications)
  :config
  (setq org-alert-notification-title "Org alert reminder"
        org-alert-interval 300
        org-alert-notify-cutoff 5
        org-alert-notify-after-event-cutoff 0)
  (org-alert-enable))

;; code block creation and execution
(setq org-confirm-babel-evaluate nil)

(org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t)
    (shell . t)
    (python . t)))

(add-to-list 'org-modules 'org-tempo)
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("sh" . "src sh"))
(add-to-list 'org-structure-template-alist '("bash" . "src bash"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("java" . "src java"))
(add-to-list 'org-structure-template-alist '("js" . "src js"))

;; knowledge management system
(use-package org-roam
  :custom
  (org-roam-directory "~/Documents/org-roam/")
  (org-roam-capture-templates
   '(("d" "default" plain
      "#+filetags: %^G:develop:\n\nPrevious:\nRelated to:\n\n* Questions\n\n- %?\n\n* Body\n\n* References"
      :target
      (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("m" "map" plain
      "#+filetags: :map%^G:develop:\n\nPrevious:\nRelated to:\n\n* Body\n\n- %?"
      :target
      (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)))
  (org-roam-node-display-template (concat "${title:*} " (propertize "${tags:50}" 'face 'org-tag)))
  :config
  ;; use hyphens instead of underscores in the file creation
  (cl-defmethod org-roam-node-slug ((node org-roam-node))
    "Return the slug of NODE."
    (let ((title (org-roam-node-title node))
          (slug-trim-chars '(768 769 770 771 772 774 775 776 777 778 779 780 795 803 804 805 807 813 814 816 817)))
      (cl-flet* ((nonspacing-mark-p (char) (memq char slug-trim-chars))
                 (strip-nonspacing-marks (s) (string-glyph-compose
                                              (apply #'string
                                                     (seq-remove #'nonspacing-mark-p
                                                                 (string-glyph-decompose s)))))
                 (cl-replace (title pair) (replace-regexp-in-string (car pair) (cdr pair) title)))
        (let* ((pairs `(("[^[:alnum:][:digit:]]" . "-")
                        ("--*" . "-")
                        ("^-" . "")
                        ("-$" . "")))
               (slug (-reduce-from #'cl-replace (strip-nonspacing-marks title) pairs)))
          (downcase slug)))))

  (unless (file-directory-p "~/Documents/org-roam/")
    (make-directory (expand-file-name "~/Documents/org-roam/")))
  (org-roam-db-autosync-mode))

(use-package org-roam-ui
  :after org-roam)
