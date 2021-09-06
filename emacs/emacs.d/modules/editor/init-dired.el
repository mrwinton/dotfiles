;;; init-dired.el --- Dired customisations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package dired
  :straight (:type built-in)
  :after exec-path-from-shell
  :custom
  (dired-listing-switches "-ahl --group-directories-first")
  (dired-dwim-target t)
  (dired-use-ls-dired t)
  (dired-recursive-deletes 'always)
  (dired-recursive-copies 'always)
  (dired-clean-up-buffers-too t)
  (insert-directory-program (executable-find "gls")))

(use-package diff-hl
  :hook (dired-mode . diff-hl-dired-mode))

(provide 'init-dired)
;;; init-dired.el ends here
