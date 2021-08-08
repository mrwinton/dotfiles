;;; init-flycheck.el --- flycheck customisations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package flycheck
  :defer 1
  :commands (flycheck-list-errors flycheck-buffer flycheck-add-next-checker)
  :config
  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)
  (setq flycheck-check-syntax-automatically '(save idle-change new-line mode-enabled))
  (global-flycheck-mode))

(use-package flycheck-posframe
  :after flycheck
  :hook (flycheck-mode . flycheck-posframe-mode)
  :config
  (flycheck-posframe-configure-pretty-defaults))

(provide 'init-flycheck)
;;; init-flycheck.el ends here
