;;; init-snippets.el --- snippets -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package yasnippet
  :after company
  :hook ((org-mode rspec-mode ruby-mode enh-ruby-mode) . yas-minor-mode)
  :config

  ;; Use only own snippets, do not use bundled ones
  (setq yas-snippet-dirs (list (expand-file-name "~/.emacs.d/snippets")))

  ;; Don't mess with the indentation
  (setq yas-indent-line 'fixed)

  ;; No need to be so verbose
  (setq yas-verbosity 1)

  (yas-reload-all)

  (add-to-list 'company-backends 'company-yasnippet)

  (add-hook 'snippet-mode-hook
            (lambda ()
              ;; Temporarily disable required newline at the end of file
              ;; This fixes the problem with an extra newline when expanding snippets
              (setq-local require-final-newline nil))))

(provide 'init-snippets)
