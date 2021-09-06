;;; init-yaml.el --- yaml -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package yaml-mode
  :mode ("\\.yml\\'" "\\.yaml\\'" "\\.yaml.erb\\'"))

(use-package yaml-tomato
  :mode ("\\.yml\\'" "\\.yaml\\'" "\\.yaml.erb\\'"))

(provide 'init-yaml)
;;; init-yaml.el ends here
