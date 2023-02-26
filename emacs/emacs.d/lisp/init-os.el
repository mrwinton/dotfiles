;;; init-os.el --- os configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Uses system trash rather than deleting forever
(setq delete-by-moving-to-trash t)
(if (eq system-type 'darwin)
    (setq trash-directory "~/.Trash"))

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

(provide 'init-os)
;;; init-os.el ends here
