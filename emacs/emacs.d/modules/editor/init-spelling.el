;;; init-spelling.el --- spelling customisations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setenv "DICTIONARY" "en_GB")
(use-package ispell
  :after exec-path-from-shell
  :config
  (let ((executable (executable-find "hunspell")))
    (when executable
      (setq-default ispell-program-name executable)
      (setq ispell-command-name "hunspell"
            ispell-dictionary "en_GB"
            ispell-really-hunspell t
            ispell-extra-args '("-a" "-i" "utf-8")))))

(provide 'init-spelling)
;;; init-spelling.el ends here
