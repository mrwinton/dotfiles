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

(use-package doom-themes
  :disabled
  :custom
  doom-themes-enable-bold t
	doom-themes-enable-italic t
  :config
  (doom-themes-org-config))

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

(defcustom mrwinton/dark-theme 'modus-vivendi
  "The theme to enable when dark-mode is active."
  :type 'symbol)

(defcustom mrwinton/light-theme 'modus-operandi
  "The theme to enable when dark-mode is inactive."
  :type 'symbol)

(defcustom mrwinton/polling-interval-seconds 60
  "The number of seconds between which to poll for dark mode state."
  :type 'integer)

(defvar mrwinton/last-dark-mode-state 'unknown)

(defun mrwinton/is-dark-mode ()
  "Invoke applescript using Emacs built-in AppleScript support to
see if dark mode is enabled. Return true if it is."
  (string-equal "true" (ns-do-applescript "tell application \"System Events\"
	tell appearance preferences
		if (dark mode) then
			return \"true\"
		else
			return \"false\"
		end if
	end tell
end tell")))

(defun mrwinton/check-and-set-dark-mode ()
  "Set the theme according to macOS's dark mode state. Only set the
theme if current theme does not match, this prevents flickering."
  (let ((is-dark-mode (mrwinton/is-dark-mode)))
    (if (not (eq is-dark-mode mrwinton/last-dark-mode-state))
        (progn
          (setq mrwinton/last-dark-mode-state is-dark-mode)
          (if is-dark-mode
              (progn
                (load-theme mrwinton/dark-theme t)
                (disable-theme mrwinton/light-theme))
            (progn
              (load-theme mrwinton/light-theme t)
              (disable-theme mrwinton/dark-theme)))))))

(run-with-idle-timer 0 mrwinton/polling-interval-seconds 'mrwinton/check-and-set-dark-mode)

(provide 'init-ui)
;;; init-ui.el ends here
