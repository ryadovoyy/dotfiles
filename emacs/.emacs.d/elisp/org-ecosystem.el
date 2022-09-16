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
    (set-face-attribute (car face) nil :font "JetBrainsMonoNL Nerd Font"
                                       :weight 'bold
                                       :height (cdr face))))

(use-package org
  :hook (org-mode . core/org-setup)
  :config
  (setq org-ellipsis " "
        org-hide-emphasis-markers t)
  (core/org-font-setup))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("" "" "" "" "" "")))

(use-package visual-fill-column)
