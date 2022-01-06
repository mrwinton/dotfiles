;;; init-snippets.el --- snippets -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package yasnippet
  :hook (((org-mode enh-ruby-mode ruby-mode js2-mode js-mode markdown-mode rspec-mode) . yas-minor-mode)
         (snippet-mode . (lambda ()
                           ;; Temporarily disable required newline at the end of
                           ;; file This fixes the problem with an extra newline
                           ;; when expanding snippets
                           (setq-local require-final-newline nil))))
  :custom
  (yas-snippet-dirs (list (expand-file-name "snippets/" user-emacs-directory)))
  (yas-indent-line 'auto) ;; Indent using major mode
  (yas-verbosity 1) ;; Tone down verbosity
  :config
  (yas-reload-all))

(provide 'init-snippets)
;;; init-snippets.el ends here
