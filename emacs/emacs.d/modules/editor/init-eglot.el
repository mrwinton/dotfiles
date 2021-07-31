;;; init-eglot.el --- eglot customisations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package eglot
  :hook ((ruby-mode enh-ruby-mode) . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs '(enh-ruby-mode "solargraph" "socket" "--port" :autoport))
  (setq eglot-autoshutdown t
        eglot-sync-connect nil
        eglot-autoreconnect nil))

(provide 'init-eglot)
;;; init-eglot.el ends here
