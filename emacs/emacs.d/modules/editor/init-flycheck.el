;;; init-flycheck.el --- flycheck customisations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package flycheck
  :commands (flycheck-mode flycheck-list-errors flycheck-buffer flycheck-add-next-checker)
  :hook (prog-mode . flycheck-mode)
  :custom
  (flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)
  (flycheck-check-syntax-automatically '(save idle-change new-line mode-enabled))
  (flycheck-indication-mode 'right-fringe)
  (flycheck-highlighting-mode nil)
  :config
  (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
    [16 48 112 240 112 48 16] nil nil 'center)
  (global-flycheck-mode))

(use-package flycheck-posframe
  :hook (flycheck-mode . flycheck-posframe-mode)
  :config
  (flycheck-posframe-configure-pretty-defaults))

(provide 'init-flycheck)
;;; init-flycheck.el ends here
