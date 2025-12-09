;;; init-ui.el --- ui configuration  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(set-frame-font "JetBrains Mono-12")

;; Enable ligatures and symbol prettification
(when (>= emacs-major-version 28)
  (setq-default prettify-symbols-alist
                '(("lambda" . "λ")
                  ("->" . "→")
                  ("=>" . "⇒")
                  (">=" . "≥")
                  ("<=" . "≤")
                  ("!=" . "≠")
                  ("==" . "═")
                  ("===" . "≡")
                  ("/=" . "≠")))
  (global-prettify-symbols-mode 1))

;; Go fullscreen on startup
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; Focus the emacs window in the foreground
(when (display-graphic-p)
  (select-frame-set-input-focus (selected-frame)))

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
  "Return formatted frame title showing file path and project name.
Displays the abbreviated file path and, if in a projectile project,
appends the project name in brackets."
  (let ((file buffer-file-name))
    (if file
        (concat (abbreviate-file-name file)
                (when (and (featurep 'projectile)
                           (bound-and-true-p projectile-mode)
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

(defun mrw/toggle-theme ()
  "Toggle between light and dark themes."
  (interactive)
  (if (eq (car custom-enabled-themes) mrw/dark-theme)
      (modus-themes-load-theme mrw/light-theme)
    (modus-themes-load-theme mrw/dark-theme)))

;; Load the light theme using modus-themes
(modus-themes-load-theme mrw/light-theme)


(provide 'init-ui)
;;; init-ui.el ends here
