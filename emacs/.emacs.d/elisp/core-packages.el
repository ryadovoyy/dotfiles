;;; package management

;; initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

;; set the installation folder for packages
(setq package-user-dir (expand-file-name "elpa/" user-emacs-directory))
(setq package-gnupghome-dir (expand-file-name "elpa/gnupg/" user-emacs-directory))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; initialize use-package on non-linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)

;; cause packages to be installed automatically if not already present on the system
(setq use-package-always-ensure t)

;;; packages

;; convenient and unified interface for key definitions
(use-package general
  :config
  (general-create-definer core/leader-key-def
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC"))

;; keybinding panel (displays available keybindings in a popup)
(use-package which-key
  :init
  (which-key-mode)
  :config
  (setq which-key-idle-delay 1))

;; generic completion system
(use-package ivy
  :config
  (ivy-mode)
  ;; set completion styles and switch buffer faces
  (setq ivy-re-builders-alist
        '((counsel-rg . ivy--regex-plus)
          (swiper . ivy--regex-plus)
          (t . ivy--regex-fuzzy))
        ivy-switch-buffer-faces-alist
        '((dired-mode . ivy-subdir))))

;; more friendly interface for ivy (shows documentation strings and keybinds in counsel)
(use-package ivy-rich
  :init (ivy-rich-mode))

;; better fuzzy matching for ivy
(use-package flx)

;; collection of ivy-enhanced versions of common Emacs commands
(use-package counsel
  :custom
  (counsel-rg-base-command
    "rg -SHnM 240 --no-heading --color never --hidden --glob '!.git/*' %s"))

;; telescope-like project interaction
(use-package projectile
  :init
  (when (file-directory-p "~/projects")
    (setq projectile-project-search-path '("~/projects")))
  (setq projectile-switch-project-action #'projectile-dired)
  :custom
  (projectile-completion-system 'ivy)
  :config
  (projectile-mode))

;; git integration
(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; work with git forges (github or gitlab)
;; (use-package forge
;;   :after magit)

;; extensible vi layer
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
        evil-insert-state-message nil)
  :config
  (evil-mode)
  (evil-set-initial-state 'dashboard-mode 'normal)

  ;; highlight on yank
  (defun core/evil-yank-advice (orig-fn beg end &rest args)
    (pulse-momentary-highlight-region beg end)
    (apply orig-fn beg end args))

  (advice-add 'evil-yank :around #'core/evil-yank-advice))

(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode))

;; evil-mode keybindings for different emacs modes
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; better *help* buffer
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable))

;; best terminal emulator
(use-package vterm
  :commands vterm
  :config
  (setq vterm-timer-delay nil
        vterm-max-scrollback 10000))

(use-package multi-vterm
  :after vterm
  :custom
  (multi-vterm-buffer-name "vterm"))

;; mode line
(use-package doom-modeline
  :init
  (doom-modeline-mode)
  :config
  (setq doom-modeline-height 35
        doom-modeline-bar-width 5
        doom-modeline-buffer-file-name-style 'file-name
        doom-modeline-workspace-name nil
        doom-modeline-modal-icon nil
        doom-modeline-major-mode-icon nil))

;; colorize color names
(use-package rainbow-mode
  :commands rainbow-mode)

;; themes
(use-package doom-themes
  :custom
  (doom-themes-enable-italic nil)
  :config
  (load-theme 'doom-wilmersdorf t)
  (doom-themes-org-config))

(use-package paren
  :custom
  (show-paren-delay 0)
  :config
  (set-face-attribute 'show-paren-match nil :weight 'normal))

(use-package pulse
  :config
  (set-face-background 'pulse-highlight-face (doom-color 'blue))
  (set-face-background 'pulse-highlight-start-face (doom-color 'blue)))

;; icons
(use-package all-the-icons
  :if (display-graphic-p)
  :init
  (unless (find-font (font-spec :name "all-the-icons"))
    (all-the-icons-install-fonts)))

;; workspaces
(use-package tab-bar
  :custom
  (tab-bar-format '(tab-bar-format-tabs tab-bar-separator))
  (tab-bar-close-button-show nil)
  (tab-bar-show 1)
  :config
  (set-face-attribute 'tab-bar nil
    :background (doom-color 'modeline-bg)
    :foreground (doom-color 'modeline-bg))
  (set-face-attribute 'tab-bar-tab nil
    :background (doom-color 'modeline-bg)
    :foreground (doom-color 'blue))
  (set-face-attribute 'tab-bar-tab-inactive nil
    :background (doom-color 'modeline-bg)
    :foreground (doom-color 'fg)))
