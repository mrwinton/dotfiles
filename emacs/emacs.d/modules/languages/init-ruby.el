;;; init-ruby.el --- ruby -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package enh-ruby-mode
  :after exec-path-from-shell
  :mode (("\\.rb\\'"       . enh-ruby-mode)
         ("\\.ru\\'"       . enh-ruby-mode)
         ("\\.jbuilder\\'" . enh-ruby-mode)
         ("\\.gemspec\\'"  . enh-ruby-mode)
         ("\\.rake\\'"     . enh-ruby-mode)
         ("Rakefile\\'"    . enh-ruby-mode)
         ("Gemfile\\'"     . enh-ruby-mode)
         ("Guardfile\\'"   . enh-ruby-mode)
         ("Capfile\\'"     . enh-ruby-mode)
         ("Vagrantfile\\'" . enh-ruby-mode))
  :config
  (setq enh-ruby-indent-level 2)
  (setq enh-ruby-hanging-paren-deep-indent-level 2)
  (setq enh-ruby-deep-indent-paren nil)
  (setq enh-ruby-add-encoding-comment-on-save nil)
  (setq ruby-insert-encoding-magic-comment nil))

(use-package ruby-end)

(use-package rspec-mode
  :after exec-path-from-shell
  :init
  (setq rspec-spec-command "bundle exec rspec")
  (setq rspec-use-bundler-when-possible nil)
  (setq rspec-use-spring-when-possible nil)
  (setq rspec-use-opts-file-when-available nil)
  (setq rspec-command-options "--color --format documentation"))

(setq ruby-align-to-stmt-keywords '(def if))

(provide 'init-ruby)
