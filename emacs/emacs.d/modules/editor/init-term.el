;;; init-term.el --- term -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package vterm
  :commands vterm
  :config
  (add-hook 'vterm-mode-hook
            #'(lambda ()
                (setq-local
                 ;; Disable font height override in vterm because it messes up the displayed text area
                 default-text-properties nil
                 ;; Disable line highlight
                 global-hl-line-mode nil))))

(provide 'init-term)
