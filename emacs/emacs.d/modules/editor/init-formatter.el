;;; init-formatter.el --- formatter customisations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package format-all
  :commands (format-all-buffer format-all-mode)
  :hook (prog-mode . format-all-mode))

(provide 'init-formatter)
;;; init-formatter.el ends here
