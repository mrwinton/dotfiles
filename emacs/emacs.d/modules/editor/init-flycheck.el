;;; init-flycheck.el --- flycheck customisations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package flycheck
  :config
  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)
  (global-flycheck-mode))

(provide 'init-flycheck)
;;; init-flycheck.el ends here
