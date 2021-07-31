;;; init-advice.el --- advice customisations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package which-key
  :init (which-key-mode)
  :config (setq which-key-idle-delay 1))

(use-package helpful
  :bind (("C-h f"   . helpful-callable)
         ("C-h k"   . helpful-key)
         ("C-h v"   . helpful-variable)
         ("C-h C-h" . helpful-at-point)))

(provide 'init-advice)
;;; init-advice.el ends here
