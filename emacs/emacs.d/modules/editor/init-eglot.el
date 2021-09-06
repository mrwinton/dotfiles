;;; init-eglot.el --- eglot customisations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package eglot
  :hook (((ruby-mode enh-ruby-mode) . eglot-ensure)
         (js2-mode . eglot-ensure))
  :custom
  (eglot-autoshutdown t)
  (eglot-sync-connect nil)
  (eglot-autoreconnect nil)
  :config
  (add-to-list 'eglot-server-programs '(enh-ruby-mode "solargraph" "socket" "--port" :autoport)))

(provide 'init-eglot)
;;; init-eglot.el ends here
