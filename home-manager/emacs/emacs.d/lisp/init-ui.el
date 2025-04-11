;;; init-ui.el --- ui configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(set-frame-font "JetBrains Mono")

;; Go fullscreen on startup
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; Focus the emacs window in the foreground
(x-focus-frame nil)

;; Highlight the active line, everywhere.
(global-hl-line-mode 1)

(use-package mode-line-bell
  :hook (after-init . mode-line-bell-mode))

(use-package modus-themes
  :demand t
  :custom
  (modus-themes-mode-line '(accented borderless))
  (modus-themes-bold-constructs t)
  (modus-themes-italic-constructs t)
  (modus-themes-scale-headings t)
  (modus-themes-lang-checkers '(text-also straight-underline))
  (modus-themes-hl-line '(accented))
  (modus-themes-region '(bg-only accented))
  (modus-themes-diffs 'desaturated)
  (modus-themes-org-blocks 'tinted-background)
  (modus-themes-headings '((1 . (no-bold overline)) (t . (no-bold))))
  (modus-themes-paren-match '(intense bold))
  (modus-themes-links '(neutral-underline)))

(use-package minions
  :hook (after-init . minions-mode))

(defun mrw/frame-title-format ()
  "Return frame title with current project name, where applicable."
  (let ((file buffer-file-name))
    (if file
        (concat (abbreviate-file-name file)
                (when (and (bound-and-true-p projectile-mode)
                           (projectile-project-p))
                  (format " [%s]" (projectile-project-name))))
      "%b")))

(when (display-graphic-p)
  (setq frame-title-format '((:eval (mrw/frame-title-format)))))

(defcustom mrw/dark-theme 'modus-vivendi
  "The theme to enable when dark-mode is active."
  :type 'symbol)

(defcustom mrw/light-theme 'modus-operandi
  "The theme to enable when dark-mode is inactive."
  :type 'symbol)

;; Just load the light theme
(load-theme mrw/light-theme t)

(provide 'init-ui)
;;; init-ui.el ends here
