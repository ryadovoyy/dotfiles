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
      (set-face-underline  'org-link t))))
