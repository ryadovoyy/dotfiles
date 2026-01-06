;;; user interface

;; remove menu bar
(menu-bar-mode -1)

;; enable line numbers for the text mode
(add-hook 'text-mode-hook (lambda () (display-line-numbers-mode)))

;; enable hybrid line numbers
(setq display-line-numbers-type 'relative)

;; disable cursor blinking
(setq blink-cursor-mode nil)

;; turn off cursors in non-selected windows
(setq-default cursor-in-non-selected-windows nil)

;; enable vim-like scrolling
(setq scroll-step 1)
(setq scroll-margin 5)

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

;; trim trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; silence compiler warnings
(setq native-comp-async-report-warnings-errors nil)

;;; package management

;; initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org"   . "https://orgmode.org/elpa/")
                         ("elpa"  . "https://elpa.gnu.org/packages/")))

;; set installation folder for packages
(setq package-user-dir (expand-file-name "elpa/" user-emacs-directory))
(setq package-gnupghome-dir (expand-file-name "elpa/gnupg/" user-emacs-directory))

;; initialize use-package
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)

;; cause packages to be installed automatically if not already present on the system
(setq use-package-always-ensure t)

;; don't check package signatures
(setq package-check-signature nil)

;;; packages

;; temporary highlighting
(use-package pulse)

;; tty clipboard
(use-package clipetty
  :hook
  (after-init . global-clipetty-mode))

;; convenient interface for key definitions
(use-package general
  :config
  (general-evil-setup)
  (general-create-definer core/leader-key-def
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC"))

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

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode))

(use-package evil-replace-with-register
  :after evil
  :custom
  (evil-replace-with-register-key "gr")
  :config
  (evil-replace-with-register-install))

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

;; theme
(use-package base16-theme
  :config
  (load-theme 'base16-default-dark t)

  ;; interface
  (set-face-background 'default "unspecified")
  (set-face-background 'line-number "unspecified")
  (set-face-background 'line-number-current-line "unspecified")
  (set-face-background 'help-key-binding "unspecified")
  (set-face-background 'mode-line "black")
  (set-face-background 'mode-line-inactive "black")
  (set-face-foreground 'vertical-border "white")

  ;; highlight
  (set-face-background 'highlight "brightblack")
  (set-face-background 'magit-section-highlight "brightblack")

  (dolist (face '(isearch
                  region
                  lazy-highlight
                  pulse-highlight-face
                  pulse-highlight-start-face))
    (set-face-attribute face nil
                        :background "brightblack"
                        :foreground "black"
                        :inverse-video nil))

  ;; org
  (add-hook 'org-mode-hook
    (lambda ()
      (set-face-background 'org-block "black")
      (set-face-background 'org-block-begin-line "black")
      (set-face-foreground 'org-hide "black")
      (set-face-foreground 'org-drawer "brightblack")
      (set-face-underline  'org-link t))))

;;; keybindings

;; quit prompts
(general-define-key
  "<escape>" 'keyboard-escape-quit)

;; minibuffer movement
(general-define-key
  :keymaps 'icomplete-fido-mode-map
  "TAB"       'icomplete-forward-completions
  "<backtab>" 'icomplete-backward-completions)

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
(core/leader-key-def
  "o"   '(:ignore t :which-key "org")
  "ol"  '(org-insert-link :which-key "insert/edit link")
  "oo"  '(org-open-at-point :which-key "open link")
  "or"  '(:ignore t :which-key "roam")
  "orn" '(org-roam-node-find :which-key "open node")
  "orl" '(org-roam-node-insert :which-key "insert link to another node")
  "orb" '(org-roam-buffer-toggle :which-key "toggle backlinks buffer")
  "org" '(org-roam-ui-mode :which-key "view graph in browser"))
