;;; init-formatter.el --- formatter customisations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Package `apheleia' implements a sophisticated algorithm for
;; applying code formatters asynchronously on save without moving
;; point or modifying the scroll position.
;;(use-package apheleia
;;  :init
;;  (apheleia-global-mode +1))

(use-package format-all
  :after (exec-path-from-shell))

(provide 'init-formatter)
;;; init-formatter.el ends here
