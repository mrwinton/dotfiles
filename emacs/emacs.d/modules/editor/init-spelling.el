;;; init-spelling.el --- spelling customisations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq ispell-program-name "/run/current-system/sw/bin/hunspell")
(setq ispell-dictionary "en_GB")
(setenv "DICTIONARY" "en_GB")

(use-package spell-fu
  :after exec-path-from-shell
  :config
  (global-spell-fu-mode))

(provide 'init-spelling)
;;; init-spelling.el ends here
