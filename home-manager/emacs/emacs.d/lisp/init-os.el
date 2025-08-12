;;; init-os.el --- os configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;; macOS-specific settings
(when (eq system-type 'darwin)
  (setq trash-directory "~/.Trash")
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none)
  ;; Disable `ns-popup-font-panel', which causes emacs to sometimes freeze
  (global-unset-key (kbd "s-t"))

  ;; Mouse wheel configuration
  (setq mouse-wheel-scroll-amount '(1
                                    ((shift) . 5)
                                    ((control))))

  ;; Disable horizontal scrolling with mouse wheel
  (global-set-key (kbd "<wheel-right>") 'ignore)
  (global-set-key (kbd "<wheel-left>") 'ignore)
  (global-set-key (kbd "<double-wheel-right>") 'ignore)
  (global-set-key (kbd "<double-wheel-left>") 'ignore)
  (global-set-key (kbd "<triple-wheel-right>") 'ignore)
  (global-set-key (kbd "<triple-wheel-left>") 'ignore)

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
