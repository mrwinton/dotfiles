;;; init.el --- Initialize my biggest bike shed -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into a number of
;; other files.

;;; Code:

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "modules/editor" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "modules/languages" user-emacs-directory))

(require 'init-startup)
(require 'init-defaults)
(require 'init-os)
(require 'init-keybindings)
(require 'init-path)
(require 'init-utils)
(require 'init-ui)
(require 'init-git)
(require 'init-text)
(require 'init-minibuffer)
(require 'init-completion)
(require 'init-langs)
(require 'init-org)

;; Run GC when idle
(run-with-idle-timer 10 nil
                     (lambda ()
                       "Clean up gc."
                       (setq gc-cons-threshold  67108864) ; 64M
                       (setq gc-cons-percentage 0.1) ; original value
                       (garbage-collect)))

;; Extensions
(require 'init-local nil t)

(provide 'init)
;;; init.el ends here
