;;; init-advice.el --- advice customisations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Package `which-key' displays the key bindings and associated
;; commands following the currently-entered key prefix in a popup.
(use-package which-key
  :defer 1
  :custom
  (which-key-idle-delay 1)
  :config
  (which-key-mode))

(use-package helpful
  :commands (helpful-callable helpful-key helpful-variable helpful-command helpful-at-point)
  :bind (("C-h f"   . helpful-callable)
         ("C-h k"   . helpful-key)
         ("C-h v"   . helpful-variable)
         ("C-h x"   . helpful-command)
         ("C-h C-h" . helpful-at-point)))

(provide 'init-advice)
;;; init-advice.el ends here
