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

;; enable the fill-column indicator
(setq-default fill-column 80)
(set-face-attribute 'fill-column-indicator nil :inherit 'whitespace-tab)
(add-hook 'prog-mode-hook (lambda () (display-fill-column-indicator-mode)))

;; enable window dividers
(setq window-divider-default-places t)
(setq window-divider-default-right-width 2)
(setq window-divider-default-bottom-width 2)
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
  (add-hook mode (lambda () (display-line-numbers-mode))))

;; disable cursor blinking
(setq blink-cursor-mode nil)

;; vim-like scrolling
(setq scroll-step 1)
(setq scroll-margin 8)

;;; editor

;; use spaces instead of tabs when indenting
(setq-default indent-tabs-mode nil)

;; display 4 spaces for every TAB character
(setq-default tab-width 4)
(defvaralias 'c-basic-offset 'tab-width)

;;; keep .emacs.d and other folders clean

;; set the custom emacs directory
(setq user-emacs-directory (expand-file-name "~/.local/share/emacs/"))

;; store customizations in a separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; create and set the session directory
(defvar core/session-dir (expand-file-name "sessions/" user-emacs-directory))

(make-directory core/session-dir t)
(setq desktop-path `(,core/session-dir))
(setq desktop-dirname core/session-dir)

;; set the native comp cache directory
(add-to-list 'native-comp-eln-load-path
             (expand-file-name "eln-cache/" user-emacs-directory))

;; disable creating backup, auto-save and lock files
(setq make-backup-files nil
      auto-save-default nil
      auto-save-list-file-prefix nil
      create-lockfiles nil)

;;; other settings

;; follow symbolic links and visit real files
(setq vc-follow-symlinks t)

;; prevent garbage collection at startup to cut down startup time
(setq gc-cons-threshold most-positive-fixnum)

;; reduce the frequency of GC after startup by setting the threshold to 16mb
(add-hook 'emacs-startup-hook (lambda ()
  (setq gc-cons-threshold (* 16 1024 1024))))

;; prevent GC while minibuffer is open (improves performance)
(defun core/minibuffer-setup ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun core/minibuffer-exit ()
  ;; defer it so that commands launched immediately after will enjoy the benefits
  (run-at-time 1 nil (lambda ()
    (setq gc-cons-threshold (* 16 1024 1024)))))

(add-hook 'minibuffer-setup-hook #'core/minibuffer-setup)
(add-hook 'minibuffer-exit-hook #'core/minibuffer-exit)

;; increase the amount of data which emacs reads from the process to 1mb (for lsp)
(setq read-process-output-max (* 1024 1024))

;; silence compiler warnings
(setq native-comp-async-report-warnings-errors nil)
