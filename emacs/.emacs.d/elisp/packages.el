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
  ;; set the fuzzy completion style
  (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy))))

;; more friendly interface for ivy (shows documentation strings and keybinds in counsel)
(use-package ivy-rich
  :init (ivy-rich-mode))

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
(use-package forge
  :after magit)

;; extensible vi layer
(use-package evil
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-want-C-u-scroll t
        evil-want-Y-yank-to-eol t
        evil-respect-visual-line-mode t
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

;; mode line
(use-package doom-modeline
  :init
  (doom-modeline-mode)
  :config
  (setq doom-modeline-height 35
        doom-modeline-bar-width 5
        doom-modeline-buffer-file-name-style 'file-name
        doom-modeline-modal-icon nil
        doom-modeline-major-mode-icon nil))

;; themes
(use-package doom-themes
  :config
  (load-theme 'doom-wilmersdorf t)
  (doom-themes-org-config))

;; icons
(use-package all-the-icons
  :if (display-graphic-p)
  :init
  (unless (find-font (font-spec :name "all-the-icons"))
    (all-the-icons-install-fonts)))
