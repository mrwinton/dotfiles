;;; init.el --- Initialize my biggest bike shed -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into a number of
;; other files.

;;; Code:

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
(setq debug-on-error t)
(add-hook 'after-init-hook '(lambda () (setq debug-on-error nil)))

;; Set and forget UTF-8, no need to include in each file.
(set-language-environment "UTF-8")

;; Require the configuration files
(add-to-list 'load-path (expand-file-name "modules/core" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "modules/editor" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "modules/languages" user-emacs-directory))

;; Core
(require 'init-bootstrap)
(require 'init-gcmh)
(require 'init-no-littering)
(require 'init-defaults)
(require 'init-macos)
(require 'init-exec-path)
(require 'init-direnv)

;; Editor
(require 'init-advice)
(require 'init-company)
(require 'init-completion)
(require 'init-dired)
(require 'init-eglot)
(require 'init-flycheck)
(require 'init-formatter)
(require 'init-magit)
(require 'init-projectile)
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

(provide 'init)

;;; init.el ends here
