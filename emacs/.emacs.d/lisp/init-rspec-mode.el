;;; Basic rspec setup
(require-package 'rspec-mode)

(require 'rspec-mode)
(setq rspec-spec-command " bundle exec rspec")
(setq rspec-use-bundler-when-possible nil)
(setq rspec-use-spring-when-possible nil)
(setq rspec-command-options "--color --format documentation")

(add-hook 'after-init-hook 'inf-ruby-switch-setup)

(eval-after-load 'rspec-mode
  '(rspec-install-snippets))

(provide 'init-rspec-mode)
