;;; init-css.el --- css -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package css-mode
  :mode (("\\.css\\'" . css-mode))
  :custom
  (css-indent-offset 2))

(use-package scss-mode
  :mode (("\\.scss\\'" . scss-mode))
  :custom
  (scss-compile-at-save nil))

(provide 'init-css)
;;; init-css.el ends here
