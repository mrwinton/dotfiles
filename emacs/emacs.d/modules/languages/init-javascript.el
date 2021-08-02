;;; init-javascript.el --- javascript -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package js2-mode
  :mode (("\\.js\\'" . js2-mode)
         ("\\.prettierrc'" . js2-mode))
  :config
  (setq js-indent-level 2
        js-basic-offset 2
        tab-width 2
        ;; Flycheck provides these features, so disable them: conflicting with
        ;; the eslint settings.
        js2-strict-trailing-comma-warning nil
        js2-strict-missing-semi-warning nil))

(provide 'init-javascript)
