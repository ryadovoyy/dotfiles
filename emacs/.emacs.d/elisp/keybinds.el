;;; native keybindings

;; make ESC quit prompts
(general-define-key
  "<escape>" 'keyboard-escape-quit)

(core/leader-key-def
  "h"  '(help-command :which-key "help")
  "s"  '(:ignore t :which-key "save")
  "ss" '(save-buffer :which-key "save current buffer")
  "sa" '(save-some-buffers :which-key "save several buffers"))

(core/leader-key-def
  "e"  '(:ignore t :which-key "emacs config")
  "er" '((lambda () (interactive) (load-file "~/.emacs.d/init.el")) :which-key "reload config")
  "ee" '((lambda () (interactive) (find-file "~/.emacs.d/init.el")) :which-key "edit config"))

;;; package keybindings

;; ivy, counsel, projectile
(core/leader-key-def
  "p"   '(:ignore t :which-key "telescope")
  "ps"  '(swiper :which-key "buffer search")
  "pr"  '(counsel-rg :which-key "project ripgrep")
  "pb"  '(counsel-switch-buffer :which-key "switch buffer")
  "pp"  '(projectile-switch-project :which-key "switch project")
  "pf"  '(:ignore t :which-key "open file")
  "pff" '(projectile-find-file :which-key "open project file")
  "pfw" '(projectile-find-file-other-window :which-key "open project file in another window")
  "pfg" '(counsel-find-file :which-key "open global file"))

(general-define-key
  :keymaps 'ivy-minibuffer-map
  "TAB" 'ivy-alt-done
  "C-j" 'ivy-next-line
  "C-k" 'ivy-previous-line)

(general-define-key
  :keymaps 'ivy-switch-buffer-map
  "C-k" 'ivy-previous-line
  "C-d" 'ivy-switch-buffer-kill)

(general-define-key
  :keymaps 'ivy-reverse-i-search-map
  "C-k" 'ivy-previous-line
  "C-d" 'ivy-reverse-i-search-kill)

(general-define-key
  "M-x" 'counsel-M-x)

(general-define-key
  :keymaps 'minibuffer-local-map
  "C-r" 'counsel-minibuffer-history)

;; magit
(core/leader-key-def
  "g" '(magit-status :which-key "magit"))

;; evil
(core/leader-key-def
  "w" 'evil-window-map)

;; helpful
(general-define-key
  [remap describe-key]      'helpful-key
  [remap describe-command]  'helpful-command
  [remap describe-function] 'counsel-describe-function
  [remap describe-variable] 'counsel-describe-variable)
