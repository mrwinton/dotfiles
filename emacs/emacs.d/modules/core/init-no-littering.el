;;; init-no-littering.el --- Keep Emacs's configuration directory clean -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package no-littering
  :demand t
  :init (setq auto-save-file-name-transforms
              `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))
              custom-file (no-littering-expand-etc-file-name "custom.el")))

(provide 'init-no-littering)

;;; init-no-littering.el ends here
