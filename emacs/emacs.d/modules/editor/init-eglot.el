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
  (add-to-list 'eglot-server-programs '(enh-ruby-mode "solargraph" "socket" "--port" :autoport))

  (defun mrwinton/eglot-eldoc-message-function (fmt &rest args)
    "Limit `eldoc-mode' strings to one line.
Eglot doesn't heed to `eldoc-echo-area-use-multiline-p'."
    (if-let ((str (and (stringp fmt) (apply #'format fmt args)))
             (line (car (split-string str "\n" t)))
             (limit (min (length line) (1- (frame-width)))))
        (eldoc-minibuffer-message (substring line 0 limit))
      (eldoc-minibuffer-message fmt args)))

  (defun mrwinton/eglot--managed-mode-hook ()
    "Customizations for `eglot--managed-mode'."
    ;; Setup `eldoc-mode'.
    (eglot--setq-saving eldoc-message-function
                        #'mrwinton/eglot-eldoc-message-function))

  (add-hook 'eglot--managed-mode-hook #'mrwinton/eglot--managed-mode-hook))

(provide 'init-eglot)
;;; init-eglot.el ends here
