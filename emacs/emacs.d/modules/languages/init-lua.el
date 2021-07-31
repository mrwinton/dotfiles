;;; init-lua.el --- lua -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package lua-mode
  :init
  ;; lua-indent-level defaults to 3 otherwise. Madness.
  (setq lua-indent-level 2))

(provide 'init-lua)
