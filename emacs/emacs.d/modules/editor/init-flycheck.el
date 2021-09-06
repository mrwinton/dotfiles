;;; init-flycheck.el --- flycheck customisations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package flycheck
  :commands (flycheck-mode flycheck-list-errors flycheck-buffer flycheck-add-next-checker)
  :hook (prog-mode . flycheck-mode)
  :custom
  (flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)
  (flycheck-check-syntax-automatically '(save idle-change new-line mode-enabled))
  :config
  (global-flycheck-mode))

(use-package flycheck-posframe
  :hook (flycheck-mode . flycheck-posframe-mode)
  :config
  (flycheck-posframe-configure-pretty-defaults))

(provide 'init-flycheck)
;;; init-flycheck.el ends here
