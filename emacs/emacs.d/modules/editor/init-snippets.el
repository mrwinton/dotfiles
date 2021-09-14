;;; init-snippets.el --- snippets -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package yasnippet
  :after company
  :hook (((org-mode enh-ruby-mode ruby-mode js2-mode js-mode markdown-mode rspec-mode) . yas-minor-mode)
         (snippet-mode . (lambda ()
                           ;; Temporarily disable required newline at the end of
                           ;; file This fixes the problem with an extra newline
                           ;; when expanding snippets
                           (setq-local require-final-newline nil))))
  :custom
  (yas-snippet-dirs (list (expand-file-name "~/.emacs.d/snippets")))
  (yas-indent-line 'fixed) ;; Don't mess with the indentation
  (yas-verbosity 1) ;; Tone down verbosity
  :config
  (yas-reload-all)
  (add-to-list 'company-backends 'company-yasnippet))

(provide 'init-snippets)
;;; init-snippets.el ends here
