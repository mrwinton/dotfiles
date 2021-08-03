;;; init-web.el --- web -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package web-mode
  :mode ("\\.html\\'" "\\.html.erb\\'")
  :defines company-backends
  :config
  ;; Indent by 2 spaces by default
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(provide 'init-web)
