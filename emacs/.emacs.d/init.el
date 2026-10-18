;;; -*- lexical-binding: t; -*-
;;; user interface

;; remove menu bar
(menu-bar-mode -1)

;; enable line numbers for files
(add-hook 'find-file-hook #'display-line-numbers-mode)

;; enable hybrid line numbers
(setq display-line-numbers-type 'relative)

;; disable cursor blinking
(setq visible-cursor nil)

;; turn off cursors in non-selected windows
(setq-default cursor-in-non-selected-windows nil)

;; enable vim-like scrolling
(setq scroll-step 1)
(setq scroll-margin 5)

;; remove bracket highlighting delay
(setq show-paren-delay 0)
(show-paren-mode)

;; ensure theme consistency
(setq frame-background-mode 'dark)

;;; keep .emacs.d and other directories clean

;; set custom emacs directory
(setq user-emacs-directory (expand-file-name "~/.local/share/emacs/"))

;; store customizations in a separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; set cache directory
(startup-redirect-eln-cache (expand-file-name "eln-cache/" user-emacs-directory))

;; disable creating backup, auto-save and lock files
(setq make-backup-files nil
      auto-save-default nil
      auto-save-list-file-prefix nil
      create-lockfiles nil)

;;; other settings

;; increase garbage collection threshold
(setq gc-cons-threshold (* 16 1024 1024))

;; enable interactive completion
(fido-vertical-mode)

;; automatically refresh buffers for changed files
(global-auto-revert-mode)

;; follow symbolic links and visit real files
(setq vc-follow-symlinks t)

;; use spaces instead of tabs when indenting
(setq-default indent-tabs-mode nil)

;; trim trailing whitespace on save
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; silence compiler warnings
(setq native-comp-async-report-warnings-errors nil)

;;; package management

;; expand package sources
(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; set installation directory for packages
(setq package-user-dir (expand-file-name "elpa/" user-emacs-directory))
(setq package-gnupghome-dir (expand-file-name "elpa/gnupg/" user-emacs-directory))

;; enable upgrading built-in packages
(setq package-install-upgrade-built-in t)

;; reinitialize package system to apply config changes
(package-initialize)

;; automatically install packages
(require 'use-package)

(setq use-package-always-ensure t)

;;; packages

;; tty clipboard
(use-package clipetty
  :hook
  (after-init . global-clipetty-mode))

;; convenient interface for key definitions
(use-package general
  :config
  (general-auto-unbind-keys)
  (general-evil-setup)

  ;; define prefix keys
  (general-create-definer core/leader-key-def
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-@"))

;; keybinding panel
(use-package which-key
  :init
  (which-key-mode)
  :config
  (setq which-key-idle-delay 1))

;; git integration
(use-package diff-hl
  :init
  (global-diff-hl-mode)
  :config
  (diff-hl-margin-mode)
  (diff-hl-flydiff-mode))

;; vi layer
(use-package evil
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-want-C-u-scroll t
        evil-want-Y-yank-to-eol t
        evil-respect-visual-line-mode t
        evil-split-window-below t
        evil-vsplit-window-right t
        evil-undo-system 'undo-redo
        evil-shift-width 2
        evil-insert-state-cursor 'box
        evil-insert-state-message nil
        evil-visual-state-message nil)
  :config
  (evil-mode)

  ;; highlight on yank
  (defun core/evil-yank-advice (orig-fn beg end &rest args)
    (pulse-momentary-highlight-region beg end)
    (apply orig-fn beg end args))

  (advice-add 'evil-yank :around #'core/evil-yank-advice))

;; org mode
(use-package org
  :mode
  ("\\.org\\'" . org-mode)
  :hook
  (org-mode . org-indent-mode)
  (org-mode . visual-line-mode)
  :config
  (setq org-hide-leading-stars t
        org-hide-emphasis-markers t
        org-agenda-files '("~/Documents/org-roam/tasks.org"))

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
  :after org
  :custom
  (org-roam-directory "~/Documents/org-roam/")
  (org-roam-capture-templates
   '(("d" "default" plain
      "Previous: %?\n\n* Questions\n\n-\n\n* Body\n\n* References"
      :target
      (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :empty-lines-before 1
      :unnarrowed t)
     ("m" "map" plain
      "Previous: %?\n\n* Body\n\n-"
      :target
      (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :empty-lines-before 1
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

;;; theme

;; clear cache
(when (fboundp 'clear-face-cache)
  (clear-face-cache t))

;; define and apply theme
(unless (and (fboundp 'display-graphic-p)
             (display-graphic-p))
  (require 'custom nil t)

  (unless (and (fboundp 'custom-theme-p)
               (custom-theme-p 'ansi))
    (deftheme ansi))

  (let* ((tty '((type tty) (class color) (min-colors 16)))

         (inverse-hl '(:background "brightblack" :foreground "black"))

         (inverse-highlight-faces
          '(isearch
            lazy-highlight
            match
            show-paren-match
            region
            pulse-highlight-face
            pulse-highlight-start-face))

         (inverse-highlight-specs
          (mapcar
           (lambda (face)
             `(,face ((,tty ,inverse-hl))))
           inverse-highlight-faces))

         (base-specs
          `((mode-line
             ((,tty (:background "black"))))
            (mode-line-inactive
             ((,tty (:inherit mode-line :foreground "brightblack"))))
            (mode-line-buffer-id
             ((,tty (:inherit mode-line))))
            (line-number
             ((,tty (:foreground "brightblack"))))
            (line-number-current-line
             ((,tty (:foreground "white" :weight bold))))
            (completions-first-difference
             ((,tty (:foreground "white"))))
            (help-key-binding
             ((,tty (:foreground "white"))))
            (font-lock-comment-face
             ((,tty (:foreground "brightblack"))))
            (highlight
             ((,tty (:background "brightblack"))))
            (magit-section-highlight
             ((,tty (:inherit highlight))))
            (org-hide
             ((,tty (:foreground "black"))))
            (org-block
             ((,tty (:background "black"))))
            (org-block-begin-line
             ((,tty (:inherit org-block :foreground "brightblack"))))
            (org-roam-olp
             ((,tty (:foreground "white")))))))

    (apply #'custom-theme-set-faces
           'ansi
           (append inverse-highlight-specs base-specs)))

  (provide-theme 'ansi)
  (enable-theme 'ansi)

  (set-face-background 'default "unspecified-bg"))

;;; keybindings

;; quit prompts
(general-define-key
  "<escape>" 'keyboard-escape-quit)

;; file and buffer operations
(general-define-key
  :states '(normal)
  :keymaps 'xref--xref-buffer-mode-map
  "RET" 'xref-goto-xref)

(core/leader-key-def
  "f"  '(project-find-file :which-key "open file")
  "/"  '(project-find-regexp :which-key "grep")
  "b"  '(switch-to-buffer :which-key "open buffer")
  "s"  '(:ignore t :which-key "save")
  "sk" '(save-buffer :which-key "save current buffer"))

;; git
(core/leader-key-def
  "h"  '(:ignore t :which-key "hunk")
  "hv" '(diff-hl-show-hunk :which-key "view hunk")
  "hn" '(diff-hl-next-hunk :which-key "next hunk")
  "hp" '(diff-hl-previous-hunk :which-key "previous hunk")
  "hr" '(diff-hl-revert-hunk :which-key "revert hunk"))

;; evil
(general-imap "j"
  (general-key-dispatch 'self-insert-command
    :timeout 0.25
    "k" 'evil-normal-state))

(core/leader-key-def
  "w"   'evil-window-map
  "q"   '(evil-quit :which-key "quit")
  "SPC" '(evil-switch-to-windows-last-buffer :which-key "toggle between buffers"))

;; org
(general-define-key
  :states '(normal)
  :keymaps 'org-mode-map
  "TAB" 'org-cycle)

(core/leader-key-def
  "o"   '(:ignore t :which-key "org")
  "ol"  '(org-insert-link :which-key "insert/edit link")
  "oo"  '(org-open-at-point :which-key "open link")
  "oa"  '(org-cycle-agenda-files :which-key "cycle through agenda files")
  "or"  '(:ignore t :which-key "roam")
  "orn" '(org-roam-node-find :which-key "open node")
  "orl" '(org-roam-node-insert :which-key "insert link to another node")
  "orb" '(org-roam-buffer-toggle :which-key "toggle backlinks buffer"))
