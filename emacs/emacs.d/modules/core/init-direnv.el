;;; init-direnv.el --- Set up direnv  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package envrc
  :hook (after-init . envrc-global-mode)
  :bind (("C-c e" . envrc-command-map)))

(provide 'init-direnv)
;;; init-direnv.el ends here
