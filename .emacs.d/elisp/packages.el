;;; package management

;; initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

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
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode))

;; more friendly interface for ivy (shows documentation strings and keybinds in counsel)
(use-package ivy-rich
  :init (ivy-rich-mode 1))

;; collection of ivy-enhanced versions of common Emacs commands
(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

;; keybinding panel (displays available keybindings in a popup)
(use-package which-key
  :init
  (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

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
  :init (doom-modeline-mode 1))

;; themes
(use-package doom-themes
  :config (load-theme 'doom-one t))

;; icons
;; run (all-the-icons-install-fonts) once after installing the package
(use-package all-the-icons
  :if (display-graphic-p))
