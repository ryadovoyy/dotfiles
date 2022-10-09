;;; user interface

;; clean up user interface
(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)

;; add sign columns
(set-fringe-mode 15)

;; remove sign columns from dired
(add-hook 'dired-mode-hook (lambda () (setq left-fringe-width 0 right-fringe-width 0)))

;; enable window dividers
(setq window-divider-default-places t)
(setq window-divider-default-right-width 10)
(setq window-divider-default-bottom-width 10)
(window-divider-mode)

;; font
(set-face-attribute 'default nil :font "JetBrainsMonoNL Nerd Font" :height 120)
(add-to-list 'default-frame-alist '(font . "JetBrainsMonoNL Nerd Font-12"))

;; enable column number in the mode line
(column-number-mode)

;; enable hybrid line numbers
(setq display-line-numbers-type 'relative)

;; enable line numbers for some modes
(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

;; disable cursor blinking
(setq blink-cursor-mode nil)

;; vim-like scrolling
(setq scroll-step 1)
(setq scroll-margin 8)

;;; keep .emacs.d and other folders clean

;; set the custom emacs directory
(setq user-emacs-directory (expand-file-name "~/.local/share/emacs/"))

;; store customizations in a separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; set the native comp cache directory
(add-to-list 'native-comp-eln-load-path
             (expand-file-name "eln-cache/" user-emacs-directory))

;; disable creating backup, auto-save and lock files
(setq make-backup-files nil
      auto-save-default nil
      auto-save-list-file-prefix nil
      create-lockfiles nil)

;;; other settings

;; reduce the frequency of garbage collection by setting the threshold to 16mb
(setq gc-cons-threshold (* 16 1024 1024))

;; prevent garbage collection while minibuffer is open (improves performance)
(defun core/minibuffer-setup ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun core/minibuffer-exit ()
  (setq gc-cons-threshold (* 16 1024 1024)))

(add-hook 'minibuffer-setup-hook #'core/minibuffer-setup)
(add-hook 'minibuffer-exit-hook #'core/minibuffer-exit)

;; increase the amount of data which emacs reads from the process to 1mb (for lsp)
(setq read-process-output-max (* 1024 1024))

;; silence compiler warnings
(setq native-comp-async-report-warnings-errors nil)
