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
