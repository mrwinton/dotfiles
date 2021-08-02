;;; init-ui.el --- ui -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Optimization
(setq idle-update-delay 1.0)

(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

(setq fast-but-imprecise-scrolling t)
(setq redisplay-skip-fontification-on-input t)

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

(use-package mode-line-bell
  :demand t
  :config
  (mode-line-bell-mode 1))

;; Go fullscreen on startup
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; Focus the emacs window in the foreground
(x-focus-frame nil)

(use-package minimal-theme
  :demand t
  :config
  (load-theme 'minimal-light t)
  (set-face-background 'highlight "gray90"))

(use-package mood-line
  :demand t
  :config
  (mood-line-mode))

(set-frame-font "Hack 12" nil t)
(setq-default cursor-type '(hbar .  2))
(setq-default cursor-in-non-selected-windows nil)

(provide 'init-ui)
