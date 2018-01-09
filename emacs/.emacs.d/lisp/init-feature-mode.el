;;; Basic feature setup
(require-package 'feature-mode)
(require 'feature-mode)

(customize-set-value 'feature-cucumber-command "bundle exec spring cucumber {feature}{options}")
(customize-set-value 'feature-root-marker-file-name ".git")

(provide 'init-feature-mode)
