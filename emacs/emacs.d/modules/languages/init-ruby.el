;;; init-ruby.el --- ruby -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package enh-ruby-mode
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
  :custom
  (enh-ruby-indent-level 2)
  (enh-ruby-hanging-paren-deep-indent-level 2)
  (enh-ruby-deep-indent-paren nil)
  (enh-ruby-add-encoding-comment-on-save nil)
  (ruby-insert-encoding-magic-comment nil)
  (ruby-align-to-stmt-keywords '(def if)))

(use-package rspec-mode
  :after enh-ruby-mode
  :custom
  (rspec-spec-command "bundle exec rspec")
  (rspec-use-bundler-when-possible nil)
  (rspec-use-spring-when-possible nil)
  (rspec-use-opts-file-when-available nil)
  (rspec-command-options "--color --format documentation"))

(provide 'init-ruby)
;;; init-ruby.el ends here
