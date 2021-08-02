;;; init-dired.el --- Dired customisations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq dired-use-ls-dired nil)
(setq dired-recursive-deletes 'top)
(setq dired-recursive-copies 'always)
(setq dired-clean-up-buffers-too t)
(setq-default dired-dwim-target t)

;; Human-readable units (KB, MB, etc) in dired buffers, directories first
(setq-default dired-listing-switches "-alh")

(use-package diff-hl
  :defer 1
  :config
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode))

(provide 'init-dired)
;;; init-dired.el ends here
