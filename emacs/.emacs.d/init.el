;;; -*- lexical-binding: t; -*-
;;; user interface

;; remove menu bar
(menu-bar-mode -1)

;; enable line numbers for the text mode
(add-hook 'text-mode-hook (lambda () (display-line-numbers-mode)))

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

;; use spaces instead of tabs when indenting
(setq-default indent-tabs-mode nil)

;; trim trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

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

;; initialize use-package
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)

;; cause packages to be installed automatically if not already present on the system
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

;; force terminal emacs to approximate colors using 16 ANSI colors
(defun core/force-16-colors (&optional frame)
  "Force FRAME, or the selected frame, to use only 16 terminal colors."
  (setq frame (or frame (selected-frame)))
  (when (and (frame-live-p frame)
             (not (display-graphic-p frame)))
    (set-frame-parameter frame 'display-color-cells 16)
    (set-frame-parameter frame 'background-mode 'dark)
    (when (boundp 'tty-color-cells)
      (setq tty-color-cells 16))))

(core/force-16-colors)
(add-hook 'after-make-frame-functions #'core/force-16-colors)

(when (fboundp 'clear-face-cache)
  (clear-face-cache t))

(unless (and (fboundp 'display-graphic-p)
             (display-graphic-p))
  (require 'custom nil t)

  ;; disable other enabled themes
  (when (boundp 'custom-enabled-themes)
    (dolist (theme (copy-sequence custom-enabled-themes))
      (unless (eq theme 'ansi)
        (ignore-errors (disable-theme theme)))))

  (unless (and (fboundp 'custom-theme-p)
               (custom-theme-p 'ansi))
    (deftheme ansi))

  (let* ((tty '((type tty) (class color) (min-colors 16)))

         ;; single style used for every highlight/match face
         (hl '(:background "brightblack" :foreground "black" :extend t))

         ;; org source block style
         (block '(:background "black" :foreground "white" :extend t))
         (block-line '(:background "black" :foreground "brightblack"
                                   :extend t :underline nil))

         (highlight-faces
          '(region
            secondary-selection
            highlight
            hl-line
            trailing-whitespace

            isearch
            isearch-fail
            lazy-highlight
            query-replace
            match
            next-error
            xref-match
            grep-match-face
            occur-match

            show-paren-match
            show-paren-match-expression
            show-paren-mismatch

            pulse-highlight-face
            pulse-highlight-start-face

            completions-highlight-face
            icomplete-highlight-face

            evil-ex-search
            evil-ex-lazy-highlight
            evil-ex-substitute-matches
            evil-ex-substitute-replacement

            magit-section-highlight))

         (highlight-specs
          (mapcar
           (lambda (face)
             `(,face ((,tty ,hl))))
           highlight-faces))

         (base-specs
          `((default
             ((,tty (:foreground nil :background nil :inverse-video nil))))

            ;; mode line
            (mode-line
             ((,tty (:background "black" :foreground "white"
                                 :box nil :inverse-video nil))))
            (mode-line-inactive
             ((,tty (:background "black" :foreground "brightblack"
                                 :box nil :inverse-video nil))))
            (mode-line-buffer-id
             ((,tty (:background "black" :foreground "brightwhite"
                                 :weight bold :box nil :inverse-video nil))))

            ;; line numbers
            (line-number
             ((,tty (:foreground "brightblack" :background nil))))
            (line-number-current-line
             ((,tty (:foreground "brightwhite" :background nil))))

            ;; links
            (link
             ((,tty (:foreground "blue" :underline t))))

            ;; org source blocks
            (org-block
             ((,tty ,block)))
            (org-block-begin-line
             ((,tty ,block-line)))
            (org-block-end-line
             ((,tty ,block-line)))

            ;; org-roam links
            (org-roam-link
             ((,tty (:inherit link))))
            (org-roam-link-auto
             ((,tty (:inherit link))))

            ;; completion UI used by fido/icomplete/*Completions*
            (completions-common-part
             ((,tty (:foreground "white"))))
            (completions-first-difference
             ((,tty (:foreground "brightwhite" :weight bold))))
            (completions-annotations
             ((,tty (:foreground "brightblack"))))

            ;; which-key
            (which-key-key-face
             ((,tty (:foreground "blue"))))
            (which-key-separator-face
             ((,tty (:foreground "brightblack"))))
            (which-key-note-face
             ((,tty (:foreground "yellow"))))
            (which-key-command-description-face
             ((,tty (:foreground "white"))))
            (which-key-local-map-description-face
             ((,tty (:foreground "white"))))
            (which-key-group-description-face
             ((,tty (:foreground "brightblue"))))
            (which-key-highlighted-command-face
             ((,tty (:foreground "brightblue" :weight bold))))
            (which-key-docstring-face
             ((,tty (:foreground "brightblack")))))))

    (apply #'custom-theme-set-faces
           'ansi
           (append highlight-specs base-specs)))

  (provide-theme 'ansi)
  (enable-theme 'ansi)

  ;; make sure the default face inherits the terminal's default fg/bg
  (set-face-attribute 'default nil
                      :foreground nil
                      :background nil
                      :inverse-video nil))

;;; keybindings

;; quit prompts
(general-define-key
  "<escape>" 'keyboard-escape-quit)

;; file and buffer operations
(core/leader-key-def
  "f"  '(find-file :which-key "open file")
  "b"  '(switch-to-buffer :which-key "open buffer")
  "s"  '(:ignore t :which-key "save")
  "sk" '(save-buffer :which-key "save current buffer"))

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
