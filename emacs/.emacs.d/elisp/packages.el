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

;; hide or abbreviate mode line displays of minor modes
(use-package diminish)

;; generic completion system
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-switch-buffer-kill))
  :config
  (ivy-mode)

  ;; set the fuzzy completion style
  (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy))))

;; more friendly interface for ivy (shows documentation strings and keybinds in counsel)
(use-package ivy-rich
  :init (ivy-rich-mode 1))

;; collection of ivy-enhanced versions of common Emacs commands
(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-switch-buffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

;; convenient and unified interface for key definitions
(use-package general
  :config
  (general-create-definer core/leader-key-def
    :keymaps '(normal emacs)
    :prefix "SPC"))

;; keybinding panel (displays available keybindings in a popup)
(use-package which-key
  :init
  (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

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
        evil-insert-state-cursor 'box)
  :config
  (evil-mode 1)

  (defun my-jk ()
    (interactive)
    (let* ((initial-key ?j)
           (final-key ?k)
           (timeout 0.5)
           (event (read-event nil nil timeout)))
      (if event
          ;; timeout met
          (if (and (characterp event) (= event final-key))
              (evil-normal-state)
            (insert initial-key)
            (push event unread-command-events))
        ;; timeout exceeded
        (insert initial-key))))

  (define-key evil-insert-state-map (kbd "j") 'my-jk)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; use visual line motions even outside of visual-line-mode buffers
  ;; (for moving between wrapped lines without skipping to other lines)
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

;; evil-mode keybindings for different emacs modes
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; better *help* buffer
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-key] . helpful-key)
  ([remap describe-command] . helpful-command)
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-variable] . counsel-describe-variable))

;; mode line
(use-package doom-modeline
  :init
  (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 35
        doom-modeline-bar-width 5
        doom-modeline-buffer-file-name-style 'file-name
        doom-modeline-modal-icon nil
        doom-modeline-major-mode-icon nil))

;; themes
(use-package doom-themes
  :config (load-theme 'doom-one t))

;; icons
(use-package all-the-icons
  :if (display-graphic-p)
  :init
  (unless (find-font (font-spec :name "all-the-icons"))
    (all-the-icons-install-fonts)))
