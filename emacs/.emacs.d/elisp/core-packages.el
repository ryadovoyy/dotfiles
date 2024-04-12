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

;; automatically update packages
(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-delete-old-versions t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe))

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

;; collection of ivy-enhanced versions of common emacs commands
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

;; best file manager
(use-package dirvish
  :after dired
  :custom
  (dired-listing-switches
   "-l --almost-all --human-readable --group-directories-first")
  (dirvish-attributes
   '(subtree-state vc-state file-size))
  (dirvish-default-layout '(1 0.13 0.53))
  (dirvish-layout-recipes
   '((0 0 0.4)
     (0 0 0.8)
     (1 0.13 0.53)))
  (dirvish-mode-line-format
   '(:left (sort file-time symlink) :right (omit yank)))
  (dirvish-mode-line-height 35)
  (dirvish-header-line-height 35)
  (dirvish-side-auto-expand nil)
  (dirvish-path-separators '(" ~" " /" "  "))
  (dirvish-vc-state-face-alist
   '((up-to-date       . nil)
     (edited           . git-gutter-fr:modified)
     (added            . git-gutter-fr:added)
     (removed          . git-gutter-fr:deleted)
     (missing          . vc-missing-state)
     (needs-merge      . dirvish-vc-needs-merge-face)
     (conflict         . vc-conflict-state)
     (unlocked-changes . vc-locked-state)
     (needs-update     . vc-needs-update-state)
     (ignored          . nil)
     (unregistered     . dirvish-vc-unregistered-face)))
  ;; kill all session buffers on quit or opening a file
  (dirvish-reuse-session nil)
  :config
  ;; fix the bar creating function to remove the side line
  (defun dirvish--bar-image (fullscreenp header)
    (when (and (display-graphic-p) (image-type-available-p 'pbm))
      (let* ((hv (if header dirvish-header-line-height dirvish-mode-line-height))
             (ht (cond ((numberp hv) hv) (fullscreenp (cdr hv)) (t (car hv)))))
        (propertize
         " " 'display
         (ignore-errors
           (create-image
            (concat (format "P1\n%i %i\n" 2 ht) "\n")
            'pbm t :foreground "None" :ascent 'center))))))

  ;; replace default directory and pdf previewers
  (dirvish-define-preview core/default (file ext)
    "Fixed default preview dispatcher for FILE."
    (when-let ((attrs (ignore-errors (file-attributes file)))
               (size (file-attribute-size attrs)))
      (cond ((file-directory-p file)
             (let* ((script `(with-current-buffer
                                 (dired-noselect ,file "-lAh --group-directories-first")
                               (buffer-string)))
                    (cmd (format "%S" `(message "\n%s" ,script))))
               `(dired . ("emacs" "-Q" "-batch" "--eval" ,cmd))))
            ((> size (or large-file-warning-threshold 10000000))
             `(info . ,(format "File %s is too big for literal preview." file)))
            ((member ext dirvish-media-exts)
             `(info . "Preview disabled for media files"))
            (t (dirvish--find-file-temporarily file)))))

  (add-to-list 'dirvish-preview-dispatchers 'core/default t)
  (setq dirvish-preview-dispatchers
        (cl-substitute 'pdf-preface 'pdf dirvish-preview-dispatchers))

  ;; improve the interface
  (add-hook 'dirvish-find-entry-hook
            (lambda (&rest _)
              ;; hide continuation lines
              (setq-local truncate-lines t)
              ;; disable parentheses highlighting
              (show-paren-local-mode -1)
              ;; set the vc gutter
              (define-fringe-bitmap 'dirvish-vc-gutter
                                    [224] nil nil '(center repeated))))
  ;; mode
  (dirvish-override-dired-mode))

;; addtional syntax highlighting for dired
(use-package diredfl
  :hook ((dired-mode dirvish-directory-view-mode) . diredfl-mode)
  :config
  (set-face-attribute 'diredfl-dir-name nil :bold t))

;; icons for dired
(use-package nerd-icons-dired
  :hook ((dired-mode dirvish-directory-view-mode) . nerd-icons-dired-mode))

;; best terminal emulator
(use-package vterm
  :commands vterm
  :config
  (setq vterm-timer-delay nil
        vterm-max-scrollback 10000))

(use-package multi-vterm
  :commands multi-vterm
  :custom
  (multi-vterm-buffer-name "vterm")
  :config
  ;; fix the buffer name formatting function
  (defun multi-vterm-format-buffer-index (index)
    "Format vterm buffer name with INDEX."
    (format "*%s%s*" multi-vterm-buffer-name index)))

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
  (dashboard-startupify-list
   '(dashboard-insert-banner
     dashboard-insert-newline
     dashboard-insert-banner-title
     dashboard-insert-newline
     dashboard-insert-init-info
     dashboard-insert-items))
  (dashboard-startup-banner 'logo)
  (dashboard-center-content t)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-icon-type 'nerd-icons)
  (dashboard-items '((projects . 5)
                     (recents . 5)
                     (agenda . 5)))
  (dashboard-item-names '(("Recent Files:" . "Recent files:")))
  (dashboard-agenda-prefix-format "%-6:c %s ")
  (dashboard-agenda-sort-strategy '(time-up))
  (dashboard-match-agenda-entry "TODO=\"TODO\"")
  :config
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  (setq dashboard-footer-messages
        (mapcar (lambda (str)
                  (concat str (make-string 5 ? )))
                dashboard-footer-messages))
  (add-hook 'dashboard-after-initialize-hook #'dashboard-refresh-buffer)
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
  (global-whitespace-mode))

;; themes
(use-package doom-themes
  :custom
  (doom-themes-enable-italic nil)
  :config
  (load-theme 'doom-tokyo-night t)
  (doom-themes-org-config)
  ;; cursor
  (set-face-background 'cursor (doom-color 'blue))
  ;; minibuffer-prompt
  (set-face-foreground 'minibuffer-prompt (doom-color 'blue))
  ;; link
  (set-face-foreground 'link (doom-color 'blue))
  ;; pulse
  (set-face-background 'pulse-highlight-face (doom-color 'blue))
  (set-face-background 'pulse-highlight-start-face (doom-color 'blue))
  ;; diff
  (set-face-foreground 'diff-indicator-added (doom-color 'vc-added))
  (set-face-foreground 'diff-indicator-removed (doom-color 'vc-deleted))
  (set-face-background 'diff-removed nil)
  ;; git-gutter-fringe
  (set-face-foreground 'git-gutter-fr:modified (doom-color 'dark-blue))
  ;; tldr
  (set-face-background 'tldr-command-itself (doom-color 'blue))
  (set-face-foreground 'tldr-code-block (doom-color 'blue))
  ;; line-number
  (set-face-background 'line-number (doom-color 'bg))
  ;; header-line
  (set-face-background 'header-line (doom-color 'bg-alt))
  ;; mode line
  (set-face-foreground 'mode-line-emphasis (doom-color 'blue))
  (set-face-background 'mode-line-highlight (doom-color 'blue))
  (set-face-background 'mode-line-buffer-id (doom-color 'blue))
  (set-face-foreground 'doom-modeline-buffer-file (doom-color 'fg))
  (set-face-foreground 'doom-modeline-info (doom-color 'blue))
  (set-face-background 'doom-modeline-bar (doom-color 'blue))
  (set-face-foreground 'doom-modeline-lsp-success (doom-color 'blue))
  (set-face-foreground 'doom-modeline-evil-normal-state (doom-color 'blue))
  (set-face-foreground 'doom-modeline-evil-insert-state (doom-color 'dark-green))
  (set-face-foreground 'doom-modeline-evil-visual-state (doom-color 'magenta))

  (set-face-bold 'doom-modeline-evil-normal-state t)
  (set-face-bold 'doom-modeline-evil-insert-state t)
  (set-face-bold 'doom-modeline-evil-visual-state t)
  (set-face-bold 'doom-modeline-evil-emacs-state t)
  (set-face-bold 'doom-modeline-evil-motion-state t)
  (set-face-bold 'doom-modeline-evil-operator-state t)
  (set-face-bold 'doom-modeline-evil-replace-state t)
  ;; highlight
  (set-face-attribute 'highlight nil
    :background (doom-color 'blue)
    :foreground (doom-color 'bg-alt))
  (set-face-attribute 'lazy-highlight nil
    :background (doom-color 'orange)
    :foreground (doom-color 'bg-alt))
  ;; vertical-border
  (set-face-attribute 'vertical-border nil
    :background (doom-color 'bg-alt)
    :foreground (doom-color 'bg-alt))
  ;; whitespace
  (set-face-attribute 'whitespace-tab nil
    :background nil
    :foreground (doom-color 'base0))
  ;; tab-bar
  (set-face-attribute 'tab-bar nil
    :background (doom-color 'modeline-bg-l)
    :foreground (doom-color 'modeline-bg-l))
  (set-face-attribute 'tab-bar-tab nil
    :background (doom-color 'modeline-bg-l)
    :foreground (doom-color 'blue))
  (set-face-attribute 'tab-bar-tab-inactive nil
    :background (doom-color 'modeline-bg-l)
    :foreground (doom-color 'fg))
  ;; org
  (custom-set-faces
    `(org-code ((t (:extend nil))))
    `(org-link ((t (:foreground ,(doom-color 'blue)))))
    `(org-list-dt ((t (:foreground ,(doom-color 'blue)))))
    `(org-todo ((t (:foreground ,(doom-color 'yellow))))))
  ;; lsp-ui
  (add-hook 'lsp-ui-mode-hook (lambda () (setq lsp-ui-doc-border (doom-color 'bg-alt)))))

;; distinguish buffers by making some darker
(use-package solaire-mode
  :after doom-themes
  :config
  (solaire-global-mode))

;; parentheses
;; <built-in>
(use-package paren
  :custom
  (show-paren-delay 0)
  :config
  (set-face-attribute 'show-paren-match nil :weight 'normal))

;; icons
(use-package nerd-icons
  :init
  (unless (find-font (font-spec :name "Symbols Nerd Font Mono"))
    (nerd-icons-install-fonts t)))
