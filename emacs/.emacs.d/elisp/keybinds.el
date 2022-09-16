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
  "er" '(core/reload-emacs-config :which-key "reload config")
  "ee" '(core/edit-emacs-config :which-key "edit config"))

;;; package keybindings

;; ivy, counsel, projectile
(core/leader-key-def
  "p"  '(:ignore t :which-key "telescope")
  "ps" '(swiper :which-key "buffer search")
  "pr" '(counsel-rg :which-key "ripgrep")
  "pf" '(core/counsel-fzf :which-key "open file")
  "pb" '(counsel-switch-buffer :which-key "switch buffer")
  "pp" '(projectile-switch-project :which-key "switch project"))

(ivy-add-actions
  'counsel-fzf
  '(("j" core/other-window "other window")))

(general-define-key
  :keymaps 'ivy-minibuffer-map
  "TAB"   'ivy-alt-done
  "C-SPC" 'ivy-dispatching-done
  "C-j"   'ivy-next-line
  "C-k"   'ivy-previous-line)

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
(general-define-key
  :keymaps 'evil-insert-state-map
  "C-h" 'evil-delete-backward-char-and-join
  "j"   'core/my-jk)

;; use visual line motions even outside of visual-line-mode buffers
;; (for moving between wrapped lines without skipping to other lines)
(general-define-key
  :keymaps 'motion
  "j" 'evil-next-visual-line
  "k" 'evil-previous-visual-line)

(core/leader-key-def
  "w" 'evil-window-map)

;; helpful
(general-define-key
  [remap describe-key]      'helpful-key
  [remap describe-command]  'helpful-command
  [remap describe-function] 'counsel-describe-function
  [remap describe-variable] 'counsel-describe-variable)
