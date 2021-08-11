;;; init-direnv.el --- Set up direnv  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package direnv
  :defer 1
  :config
  (setq direnv-always-show-summary nil)
  (direnv-mode))

(provide 'init-direnv)

;;; init-direnv.el ends here
