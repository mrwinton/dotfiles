;;; init-macos.el --- macos -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'none)
(setq mouse-wheel-scroll-amount '(1
                                  ((shift) . 5)
                                  ((control))))
(dolist (multiple '("" "double-" "triple-"))
  (dolist (direction '("right" "left"))
    (global-set-key (read-kbd-macro (concat "<"
                                            multiple "wheel-" direction ">")) 'ignore)))

;; Disable `ns-popup-font-panel', which causes emacs to sometimes freeze
(global-unset-key (kbd "s-t"))

;; Sets `ns-transparent-titlebar' and `ns-appearance' frame parameters so window
;; borders will match the enabled theme.
(use-package ns-auto-titlebar
  :demand t
  :config
  (ns-auto-titlebar-mode))

(use-package reveal-in-osx-finder
  :commands (reveal-in-osx-finder))

(use-package emacs-everywhere
  :commands (emacs-everywhere))

(provide 'init-macos)
