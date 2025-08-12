;;; init-os.el --- os configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;; macOS-specific settings
(when (eq system-type 'darwin)
  (setq trash-directory "~/.Trash")
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none)
  ;; Mouse wheel configuration
  (setq mouse-wheel-scroll-amount '(1
                                    ((shift) . 5)
                                    ((control))))

  ;; Uses system trash rather than deleting forever
  (setq delete-by-moving-to-trash t)

  ;; Sets `ns-transparent-titlebar' and `ns-appearance' frame parameters so window
  ;; borders will match the enabled theme.
  (use-package ns-auto-titlebar
    :demand t
    :config
    (ns-auto-titlebar-mode))

  (use-package reveal-in-osx-finder
    :commands (reveal-in-osx-finder)))


(provide 'init-os)
;;; init-os.el ends here
