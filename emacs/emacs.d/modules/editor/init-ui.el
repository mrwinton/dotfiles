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
  (set-face-background 'highlight "gray90")
  (set-face-background 'fringe "gray100")

  (set-face-attribute 'mode-line nil
                      :background "grey95"
                      :foreground "grey20"
                      :box '(:line-width 4 :color "grey95" :style nil)
                      :overline nil
                      :underline nil)

  (set-face-attribute 'mode-line-inactive nil
                      :background "grey98"
                      :foreground "grey50"
                      :box '(:line-width 4 :color "grey98" :style nil)
                      :overline nil
                      :underline nil)

  (set-face-attribute 'vertical-border nil
                      :foreground "gray97"))

(use-package mood-line
  :demand t
  :config
  (mood-line-mode))

(set-frame-font "Hack Nerd Font Mono 12" nil t)
(setq-default cursor-type '(hbar .  2))
(setq-default cursor-in-non-selected-windows nil)

(el-patch-feature mood-line)
(with-eval-after-load 'mood-line
  (el-patch-defun mood-line--update-vc-segment (&rest _)
    "Update `mood-line--vc-text' against the current VCS state."
    (setq mood-line--vc-text
          (when (and vc-mode buffer-file-name)
            (let ((backend (vc-backend buffer-file-name))
                  (state (vc-state buffer-file-name (vc-backend buffer-file-name))))
              (let ((face 'mode-line-neutral))
                (concat (cond ((memq state '(edited added))
                               (setq face 'mood-line-status-info)
                               (propertize (el-patch-swap "+ " " ") 'face face))
                              ((eq state 'needs-merge)
                               (setq face 'mood-line-status-warning)
                               (propertize (el-patch-swap "⟷ " " ") 'face face))
                              ((eq state 'needs-update)
                               (setq face 'mood-line-status-warning)
                               (propertize (el-patch-swap "↑ " " ") 'face face))
                              ((memq state '(removed conflict unregistered))
                               (setq face 'mood-line-status-error)
                               (propertize (el-patch-swap "✖ " " ") 'face face))
                              (t
                               (setq face 'mood-line-status-neutral)
                               (propertize (el-patch-swap "✔ " " ") 'face face)))
                        (propertize (substring vc-mode (+ (if (eq backend 'Hg) 2 3) 2))
                                    'face face
                                    'mouse-face face)
                        "  "))))))


  (el-patch-defun mood-line--update-flycheck-segment (&optional status)
    "Update `mood-line--flycheck-text' against the reported flycheck STATUS."
    (setq mood-line--flycheck-text
          (pcase status
            ('finished (if flycheck-current-errors
                           (let-alist (flycheck-count-errors flycheck-current-errors)
                             (let ((sum (+ (or .error 0) (or .warning 0))))
                               (propertize (concat (el-patch-swap "⚑ Issues: " " Issues (")
                                                   (number-to-string sum)
                                                   (el-patch-swap "  " ")  "))
                                           'face (if .error
                                                     'mood-line-status-error
                                                   'mood-line-status-warning))))
                         (propertize (el-patch-swap "✔ Good  " " Nice  ") 'face 'mood-line-status-success)))
            ('running (propertize (el-patch-swap "Δ Checking  " " Testing  ") 'face 'mood-line-status-info))
            ('errored (propertize (el-patch-swap "✖ Error  " " Hmm  ") 'face 'mood-line-status-error))
            ('interrupted (propertize (el-patch-swap "⏸ Paused  " " Pause  ") 'face 'mood-line-status-neutral))
            ('no-checker "")))))

(provide 'init-ui)
;;; init-ui.el ends here
