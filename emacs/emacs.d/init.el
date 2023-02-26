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
(require 'init-path)

;; Editor
(require 'init-advice)
(require 'init-dired)
(require 'init-eglot)
(require 'init-flycheck)
(require 'init-formatter)
(require 'init-git)
(require 'init-projectile)
(require 'init-selection)
(require 'init-snippets)
(require 'init-spelling)
(require 'init-text)
(require 'init-ui)
(require 'init-windows)

;; Languages
(require 'init-css)
(require 'init-graphql)
(require 'init-javascript)
(require 'init-json)
(require 'init-lua)
(require 'init-markdown)
(require 'init-nix)
(require 'init-org)
(require 'init-ruby)
(require 'init-sh)
(require 'init-sql)
(require 'init-web)
(require 'init-yaml)

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
