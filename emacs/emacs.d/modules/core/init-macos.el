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

(use-package reveal-in-osx-finder)

(use-package emacs-everywhere)

(provide 'init-macos)
