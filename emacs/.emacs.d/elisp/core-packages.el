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

(use-package git-gutter
  :hook ((text-mode prog-mode conf-mode) . git-gutter-mode)
  :custom
  (git-gutter:update-interval 1))

(use-package git-gutter-fringe
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

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

;; cheatsheets
(use-package tldr)

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

;; workspaces
;; <built-in>
(use-package tab-bar
  :custom
  (tab-bar-format '(tab-bar-format-tabs tab-bar-separator))
  (tab-bar-close-button-show nil)
  (tab-bar-new-tab-to 'rightmost)
  (tab-bar-new-tab-choice "*dashboard*")
  (tab-bar-tab-name-function 'tab-bar-tab-name-truncated)
  (tab-bar-show 1))

;; dashboard
(use-package dashboard
  :custom
  (dashboard-startup-banner 'logo)
  (dashboard-center-content t)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-items '((projects . 5)
                     (recents . 5)
                     (agenda . 5)))
  (dashboard-item-names '(("Recent Files:" . "Recent files:")))
  (dashboard-agenda-prefix-format "%-6:c %s ")
  (dashboard-agenda-sort-strategy '(time-up))
  (dashboard-match-agenda-entry "TODO=\"TODO\"")
  (dashboard-set-footer nil)
  :config
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  (setq dashboard-footer-messages
        (mapcar (lambda (str)
                  (concat str (make-string 5 ? )))
                dashboard-footer-messages))
  (dashboard-setup-startup-hook))

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

;; temporary highlighting
;; <built-in>
(use-package pulse)

;; themes
(use-package doom-themes
  :custom
  (doom-themes-enable-italic nil)
  :config
  (load-theme 'doom-wilmersdorf t)
  (doom-themes-org-config)
  ;; pulse
  (set-face-background 'pulse-highlight-face (doom-color 'highlight))
  (set-face-background 'pulse-highlight-start-face (doom-color 'highlight))
  ;; diff
  (set-face-foreground 'diff-indicator-added (doom-color 'vc-added))
  (set-face-foreground 'diff-indicator-removed (doom-color 'vc-deleted))
  (set-face-background 'diff-removed nil)
  ;; vertical-border
  (set-face-attribute 'vertical-border nil
    :background (doom-color 'bg-alt)
    :foreground (doom-color 'bg-alt))
  ;; tab-bar
  (set-face-attribute 'tab-bar nil
    :background (doom-color 'modeline-bg)
    :foreground (doom-color 'modeline-bg))
  (set-face-attribute 'tab-bar-tab nil
    :background (doom-color 'modeline-bg)
    :foreground (doom-color 'highlight))
  (set-face-attribute 'tab-bar-tab-inactive nil
    :background (doom-color 'modeline-bg)
    :foreground (doom-color 'fg))
  ;; mode line
  (set-face-attribute 'mode-line-inactive nil
    :background (doom-color 'bg)
    :foreground (doom-color 'bg))
  (set-face-attribute 'doom-modeline-bar-inactive nil
    :background (doom-color 'bg)
    :foreground (doom-color 'bg)))

;; parentheses
;; <built-in>
(use-package paren
  :custom
  (show-paren-delay 0)
  :config
  (set-face-attribute 'show-paren-match nil :weight 'normal))

;; blank visualization
;; <built-in>
(use-package whitespace
  :custom
  (whitespace-global-modes '(text-mode prog-mode conf-mode))
  (whitespace-display-mappings '((tab-mark 9 [9474 9] [92 9])))
  (whitespace-style
   '(face
     trailing
     tabs
     missing-newline-at-eof
     empty
     indentation
     space-after-tab
     space-before-tab
     tab-mark))
  :config
  (set-face-background 'whitespace-tab nil)
  (global-whitespace-mode))

;; icons
(use-package all-the-icons
  :if (display-graphic-p)
  :init
  (unless (find-font (font-spec :name "all-the-icons"))
    (all-the-icons-install-fonts)))
