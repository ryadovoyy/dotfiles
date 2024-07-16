;;; native keybindings

;; make ESC quit prompts
(general-define-key
  "<escape>" 'keyboard-escape-quit)

(core/leader-key-def
  "u"  '(universal-argument :which-key "universal argument")
  "h"  '(help-command :which-key "help")
  "s"  '(:ignore t :which-key "save")
  "sk" '(save-buffer :which-key "save current buffer")
  "sa" '(save-some-buffers :which-key "save several buffers")
  "sd" '(desktop-save-in-desktop-dir :which-key "save desktop")
  "sr" '(desktop-read :which-key "restore desktop"))

(core/leader-key-def
  "e"  '(:ignore t :which-key "emacs config")
  "ee" '(eval-expression :which-key "evaluate elisp expression")
  "es" '(eval-last-sexp :which-key "evaluate elisp code")
  "er" '(core/reload-emacs-config :which-key "reload config")
  "ec" '(core/edit-emacs-config :which-key "edit config"))

;;; fix non-working prefix in some modes

(general-define-key
  :keymaps '(help-mode-map dired-mode-map diff-mode-map)
  :prefix "<normal-state> SPC"
  "" nil)

(add-hook 'view-mode-hook (lambda ()
  (general-define-key
    :keymaps 'view-mode-map
    :prefix "<normal-state> SPC"
    "" nil)))

(general-define-key
  :keymaps 'org-roam-mode-map
  :prefix "SPC"
  "" nil)

;;; package keybindings

;; ivy, counsel, projectile
(core/leader-key-def
  "p"  '(:ignore t :which-key "telescope")
  "ps" '(swiper :which-key "buffer search")
  "pr" '(counsel-rg :which-key "ripgrep")
  "pf" '(core/counsel-fzf :which-key "open file")
  "pg" '(counsel-find-file :which-key "open global file")
  "pb" '(counsel-switch-buffer :which-key "switch buffer")
  "pp" '(projectile-switch-project :which-key "switch project"))

(ivy-add-actions
  'counsel-rg
  '(("s" core/counsel-rg-split "horizontal split")
    ("v" core/counsel-rg-vsplit "vertical split")))

(ivy-add-actions
  'counsel-fzf
  '(("s" core/counsel-fzf-split "horizontal split")
    ("v" core/counsel-fzf-vsplit "vertical split")))

(ivy-add-actions
  'counsel-find-file
  '(("s" core/counsel-find-file-split "horizontal split")
    ("v" core/counsel-find-file-vsplit "vertical split")))

(ivy-add-actions
  'counsel-switch-buffer
  '(("s" core/counsel-switch-buffer-split "horizontal split")
    ("v" core/counsel-switch-buffer-vsplit "vertical split")))

(ivy-add-actions
  'projectile-switch-project
  '(("s" core/projectile-switch-project-split "horizontal split")
    ("v" core/projectile-switch-project-vsplit "vertical split")))

(general-define-key
  :keymaps 'ivy-minibuffer-map
  "TAB"   'ivy-alt-done
  "C-SPC" 'ivy-dispatching-done
  "C-j"   'ivy-next-line
  "C-k"   'ivy-previous-line
  "C-n"   'next-history-element
  "C-p"   'previous-history-element)

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

;; magit, git-gutter
(core/leader-key-def
  "g"   '(:ignore t :which-key "git")
  "gs"  '(magit-status :which-key "magit status")
  "gh"  '(:ignore t :which-key "hunk")
  "ghv" '(git-gutter:popup-hunk :which-key "preview hunk")
  "ghn" '(git-gutter:next-hunk :which-key "next hunk")
  "ghp" '(git-gutter:previous-hunk :which-key "previous hunk")
  "ghs" '(git-gutter:stage-hunk :which-key "stage hunk")
  "ghr" '(git-gutter:revert-hunk :which-key "revert hunk"))

;; evil
(general-define-key
  :keymaps 'evil-insert-state-map
  "C-h" 'evil-delete-backward-char-and-join
  "j"   'core/my-jk)

(core/leader-key-def
  "w"   'evil-window-map
  "q"   '(evil-quit :which-key "quit")
  "SPC" '(evil-switch-to-windows-last-buffer :which-key "toggle between buffers"))

;; helpful
(general-define-key
  [remap describe-key]      'helpful-key
  [remap describe-command]  'helpful-command
  [remap describe-function] 'counsel-describe-function
  [remap describe-variable] 'counsel-describe-variable)

;; tldr
(core/leader-key-def
  "c" '(tldr :which-key "cheatsheets"))

;; dirvish, dired
(core/leader-key-def
  "d"   '(:ignore t :which-key "dirvish")
  "do"  '(dirvish-dwim :which-key "open dirvish")
  "ds"  '(dirvish-side :which-key "toggle sidebar")
  "dq"  '(dirvish-quit :which-key "quit dirvish session")
  "dy"  '(dirvish-yank-menu :which-key "yank menu")
  "dm"  '(dirvish-mark-menu :which-key "mark menu")
  "dn"  '(dirvish-narrow :which-key "live file filtering")
  "dl"  '(:ignore t :which-key "layout")
  "dlt" '(dirvish-layout-toggle :which-key "toggle layout")
  "dls" '(dirvish-layout-switch :which-key "switch layout"))

(general-define-key
  :keymaps 'dirvish-mode-map
  "<normal-state> TAB" 'dirvish-subtree-toggle)

(general-define-key
  :keymaps 'dired-mode-map
  "<normal-state> h" 'dired-up-directory
  "<normal-state> l" 'dired-find-file
  "<normal-state> A" 'dired-create-empty-file)

;; vterm
(core/leader-key-def
  "v"  '(:ignore t :which-key "vterm")
  "vo" '(multi-vterm :which-key "open vterm")
  "vs" '(core/vterm-split :which-key "open vterm in horizontal split")
  "vv" '(core/vterm-vsplit :which-key "open vterm in vertical split")
  "vn" '(vterm-next-prompt :which-key "next prompt")
  "vp" '(vterm-previous-prompt :which-key "previous prompt"))

;; tab-bar
(core/leader-key-def
  "t"  '(:ignore t :which-key "tabs")
  "to" '(tab-new :which-key "open tab")
  "tc" '(tab-close :which-key "close tab")
  "td" '(tab-close-other :which-key "close other tabs")
  "tu" '(tab-undo :which-key "restore last closed tab")
  "tn" '(tab-next :which-key "next tab")
  "tp" '(tab-previous :which-key "previous tab")
  "tr" '(tab-rename :which-key "rename tab"))

;; org
(core/leader-key-def
  "o"   '(:ignore t :which-key "org")
  "oi"  '(org-meta-return :which-key "insert item")
  "oh"  '(org-insert-heading-respect-content :which-key "insert heading")
  "ol"  '(org-insert-link :which-key "insert/edit link")
  "oo"  '(org-open-at-point :which-key "open at point")
  "o-"  '(org-ctrl-c-minus :which-key "(head)line to item, cycle type")
  "oa"  '(org-agenda :which-key "agenda")
  "of"  '(org-cycle-agenda-files :which-key "cycle through agenda files")
  "oe"  '(org-babel-execute-src-block :which-key "execute code block")
  "oc"  '(:ignore t :which-key "capture")
  "oci" '(org-capture :which-key "capture new item")
  "ocf" '(org-capture-finalize :which-key "finalize capture process")
  "ot"  '(:ignore t :which-key "TODO/checkbox")
  "oti" '(org-insert-todo-heading :which-key "insert TODO/checkbox")
  "ott" '(org-todo :which-key "rotate TODO state")
  "otc" '(org-toggle-checkbox :which-key "toggle checkbox")
  "otm" '(org-refile :which-key "move TODO to another heading")
  "od"  '(:ignore t :which-key "insert/edit date")
  "ods" '(org-schedule :which-key "schedule")
  "odd" '(org-deadline :which-key "deadline")
  "odt" '(org-time-stamp :which-key "time stamp")
  "or"  '(:ignore t :which-key "roam")
  "orn" '(org-roam-node-find :which-key "open node")
  "orl" '(org-roam-node-insert :which-key "insert link to another node")
  "orb" '(org-roam-buffer-toggle :which-key "toggle backlinks buffer")
  "org" '(org-roam-ui-mode :which-key "view graph in browser"))

;; company
(general-define-key
  :keymaps 'company-active-map
  "<tab>" 'company-complete-selection)
