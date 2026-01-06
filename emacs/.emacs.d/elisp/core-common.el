;;; user interface

;; clean up UI
(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)

;; prevent using UI dialogs for prompts
(setq use-dialog-box nil)

;; set font
(set-face-attribute 'default nil :font "JetBrainsMonoNL Nerd Font" :height 120)
(add-to-list 'default-frame-alist '(font . "JetBrainsMonoNL Nerd Font-12"))

;; enable line numbers for the text mode
(add-hook 'text-mode-hook (lambda () (display-line-numbers-mode)))

;; enable hybrid line numbers
(setq display-line-numbers-type 'relative)

;; disable cursor blinking
(setq blink-cursor-mode nil)

;; turn off cursors in non-selected windows
(setq-default cursor-in-non-selected-windows nil)

;; enable vim-like scrolling
(setq scroll-step 1)
(setq scroll-margin 5)

;;; keep .emacs.d and other directories clean

;; set custom emacs directory
(setq user-emacs-directory (expand-file-name "~/.local/share/emacs/"))

;; store customizations in a separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; set cache directory
(startup-redirect-eln-cache (expand-file-name "eln-cache/" user-emacs-directory))

;; disable creating backup, auto-save and lock files
(setq make-backup-files nil
      auto-save-default nil
      auto-save-list-file-prefix nil
      create-lockfiles nil)

;;; other settings

;; increase garbage collection threshold
(setq gc-cons-threshold (* 16 1024 1024))

;; enable interactive completion
(fido-vertical-mode)

;; trim trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; silence compiler warnings
(setq native-comp-async-report-warnings-errors nil)

;; disable bells
(setq ring-bell-function 'ignore)
