;; org mode
(use-package org
  :hook
  (org-mode . org-indent-mode)
  (org-mode . visual-line-mode)
  :config
  (setq org-hide-leading-stars t
        org-agenda-files '("~/Documents/org-roam/tags.org"))

  ;; code block creation
  (add-to-list 'org-modules 'org-tempo)
  (add-to-list 'org-structure-template-alist '("src" . "src"))
  (add-to-list 'org-structure-template-alist '("sh" . "src sh")))

;; org reader mode
(use-package visual-fill-column
  :hook
  (org-mode . visual-fill-column-mode)
  :custom
  (visual-fill-column-width 100)
  (visual-fill-column-center-text t)
  (visual-fill-column-enable-sensible-window-split t))

;; knowledge management system
(use-package org-roam
  :custom
  (org-roam-directory "~/Documents/org-roam/")
  (org-roam-node-display-template (concat "${title:*} " (propertize "${tags:50}" 'face 'org-tag)))
  (org-roam-capture-templates
   '(("d" "default" plain
      "#+filetags: %^G:develop:\n\nPrevious:\n\n* Questions\n\n- %?\n\n* Body\n\n* References"
      :target
      (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("m" "map" plain
      "#+filetags: :map%^G:develop:\n\nPrevious:\n\n* Body\n\n- %?"
      :target
      (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)))
  :config
  ;; use hyphens instead of underscores in filenames
  (cl-defmethod org-roam-node-slug ((node org-roam-node))
    "Return the slug of NODE."
    (require 'ucs-normalize)
    (let ((slug-trim-chars
           '(#x300 #x301 #x302 #x303 #x304 #x306 #x307
             #x308 #x309 #x30A #x30B #x30C #x31B #x323
             #x324 #x325 #x327 #x32D #x32E #x330 #x331)))
      (thread-last (org-roam-node-title node)
                   (ucs-normalize-NFD-string)
                   (seq-remove (lambda (char) (memq char slug-trim-chars)))
                   (apply #'string)
                   (ucs-normalize-NFC-string)
                   (replace-regexp-in-string "[^[:alnum:]]" "-")
                   (replace-regexp-in-string "--*" "-")
                   (replace-regexp-in-string "^-" "")
                   (replace-regexp-in-string "-$" "")
                   (downcase))))

  ;; create org roam directory
  (unless (file-directory-p "~/Documents/org-roam/")
    (make-directory (expand-file-name "~/Documents/org-roam/")))

  ;; start session
  (org-roam-db-autosync-mode))

(use-package org-roam-ui
  :after org-roam)
