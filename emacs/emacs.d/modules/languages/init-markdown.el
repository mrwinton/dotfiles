;;; init-markdown.el --- markdown -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package markdown-mode
  :mode ("/README\\(?:\\.md\\)?\\'" . gfm-mode)
  :init
  (setq markdown-enable-math t ; syntax highlighting for latex fragments
        markdown-enable-wiki-links t
        markdown-italic-underscore t
        markdown-asymmetric-header t
        markdown-fontify-code-blocks-natively t
        markdown-gfm-additional-languages '("sh")
        markdown-make-gfm-checkboxes-buttons t))

(provide 'init-markdown)
