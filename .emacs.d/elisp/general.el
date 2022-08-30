;;; user interface

;; clean up user interface
(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)

;; add a thin strip down the left and right edge of a window
(set-fringe-mode 10)

;; font
(set-face-attribute 'default nil :font "JetBrainsMonoNL Nerd Font" :height 120)

;; enable column number in the mode line
(column-number-mode)

;; enable hybrid line numbers
(setq display-line-numbers-type 'relative)

;; enable line numbers for some modes
(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

;; override some modes which derive from the above
(dolist (mode '(org-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))
