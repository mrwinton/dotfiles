;;; init-web.el --- web -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package web-mode
  :mode ("\\.html\\'" "\\.html.erb\\'")
  :defines company-backends
  :config
  ;; Autocomplete </ instantly
  (setq web-mode-enable-auto-closing t)

  ;; Indent by 2 spaces by default
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(provide 'init-web)
