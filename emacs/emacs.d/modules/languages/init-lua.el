;;; init-lua.el --- lua -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package lua-mode
  :mode (("\\.lua\\'" . lua-mode))
  :custom
  (lua-indent-level 2))

(provide 'init-lua)
;;; init-lua.el ends here
