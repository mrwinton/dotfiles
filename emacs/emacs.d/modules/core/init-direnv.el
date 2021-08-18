;;; init-direnv.el --- Set up direnv  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package envrc
  :defer 2
  :ensure t
  :commands (envrc-allow)
  :if (executable-find "direnv")
  :bind (:map envrc-mode-map
              ("C-c d" . envrc-command-map))
  :config (envrc-global-mode))

(provide 'init-direnv)

;;; init-direnv.el ends here
