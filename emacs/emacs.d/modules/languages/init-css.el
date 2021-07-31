;;; init-css.el --- css -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package css-mode
  :defer t
  :config
  (setq css-indent-offset 2))

(use-package scss-mode
  :defer t
  :config
  (setq scss-compile-at-save nil))

(provide 'init-css)
