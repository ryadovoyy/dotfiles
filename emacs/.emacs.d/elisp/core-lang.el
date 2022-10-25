;;; tools

;; lsp
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . lsp-headerline-breadcrumb-mode)
  :init
  (setq lsp-keymap-prefix "SPC l")
  :custom
  (lsp-lens-enable nil)
  (lsp-enable-symbol-highlighting nil)
  (lsp-eldoc-enable-hover nil)
  (lsp-signature-auto-activate nil)
  (lsp-modeline-code-actions-enable nil)
  (lsp-modeline-diagnostics-enable nil)
  (lsp-idle-delay 1)
  ;; breadcrumbs
  (lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-enable-diagnostics nil)
  (lsp-headerline-breadcrumb-icons-enable nil)
  :config
  (setq lsp-headerline-arrow
        #("" 0 1
          (face
           (:family "JetBrainsMonoNL Nerd Font"
            :height 1.0
            :inherit lsp-headerline-breadcrumb-separator-face)
           font-lock-face
           (:family "JetBrainsMonoNL Nerd Font"
            :height 1.0
            :inherit lsp-headerline-breadcrumb-separator-face)
           display
           (raise 0.0)
           rear-nonsticky t)))
  ;; keymap prefix
  (core/leader-key-def
    "l" lsp-command-map)
  (general-define-key
    :keymaps 'insert
    "SPC" 'self-insert-command)
  (lsp-enable-which-key-integration t)
  ;; customize the appearance of error types
  (setq flymake-fringe-indicator-position nil
        flymake-no-changes-timeout 1)
  (push '(face . nil) (get :note 'flymake-overlay-control))
  (push '(face . nil) (get :warning 'flymake-overlay-control))
  (push '(face . nil) (get :error 'flymake-overlay-control)))

(use-package lsp-ui
  :after lsp-mode
  :custom
  ;; sideline
  (lsp-ui-sideline-enable nil)
  (lsp-ui-sideline-show-diagnostics nil)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-sideline-show-code-actions nil)
  (lsp-ui-sideline-delay 1)
  ;; doc
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-doc-show-with-mouse nil)
  (lsp-ui-doc-delay 1)
  :config
  (set-face-attribute 'lsp-ui-doc-background nil :inherit 'markdown-code-face))

;; text completion framework
;; (use-package company
;;   :after lsp-mode
;;   :hook (lsp-mode . company-mode)
;;   :custom
;;   (company-minimum-prefix-length 1)
;;   (company-idle-delay 0))

;;; languages

(use-package go-mode
  :mode "\\.go\\'"
  :hook (go-mode . lsp-deferred))

(add-hook 'c-mode-common-hook (lambda () (setq indent-tabs-mode t)))

;; disable echo area display of lisp objects at point
(global-eldoc-mode -1)
