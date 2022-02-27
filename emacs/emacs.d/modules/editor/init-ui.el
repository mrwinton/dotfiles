;;; init-ui.el --- ui -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(set-frame-font "MonoLisa")

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
  (modus-themes-links '(neutral-underline))
  (modus-themes-completions 'moderate)
  :config
  (load-theme 'modus-operandi t)
  (set-face-foreground 'vertical-border (modus-themes-color 'bg-inactive)))

(use-package solar
  :straight (:type built-in)
  :demand t
  :custom
  (calendar-latitude 59.3)
  (calendar-longitude 18.1))

(use-package circadian
  :demand t
  :custom
  (circadian-themes '((:sunrise . modus-operandi)
                      (:sunset  . modus-vivendi)))
  :config
  (circadian-setup))

(use-package minions
  :hook (after-init . minions-mode))

(defun mrwinton/frame-title-format ()
  "Return frame title with current project name, where applicable."
  (let ((file buffer-file-name))
    (if file
        (concat (abbreviate-file-name file)
                (when (and (bound-and-true-p projectile-mode)
                           (projectile-project-p))
                  (format " [%s]" (projectile-project-name))))
      "%b")))

(when (display-graphic-p)
  (setq frame-title-format '((:eval (mrwinton/frame-title-format)))))

(provide 'init-ui)
;;; init-ui.el ends here
