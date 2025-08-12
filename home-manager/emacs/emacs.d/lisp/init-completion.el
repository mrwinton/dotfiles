;;; init-completion.el --- completion configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package corfu
  :hook (after-init . global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 2)
  (corfu-quit-at-boundary t)     ;; Automatically quit at word boundary
  (corfu-quit-no-match t)        ;; Automatically quit if there is no match
  (corfu-echo-documentation nil) ;; Do not show documentation in the echo area
  (corfu-cycle t)
  (corfu-preselect 'prompt)      ;; Better default selection
  :config
  (corfu-popupinfo-mode 1)       ;; Show documentation popup
  (corfu-history-mode 1))        ;; Enable completion history

(use-package cape
  :after corfu
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  :config
  (add-hook 'emacs-lisp-mode-hook
            (lambda () (add-to-list 'completion-at-point-functions #'cape-elisp-symbol t)))
  (add-hook 'org-mode-hook
            (lambda () (add-to-list 'completion-at-point-functions #'cape-tex t))))

(use-package dabbrev)

;; Add orderless for fuzzy completion matching
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package emacs
  :straight (:type built-in)
  :custom
  ;; TAB cycle if there are only few candidates
  (completion-cycle-threshold 3)
  ;; Hide commands in M-x which do not apply to the current mode
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Enable indentation+completion using the TAB key
  (tab-always-indent 'complete)
  ;; Show more details in completions (Emacs 28+)
  (completions-detailed t)
  ;; Better completion display
  (completion-show-help nil)
  (completion-auto-help 'always))

(use-package yasnippet
  :hook (((org-mode enh-ruby-mode ruby-mode js2-mode js-mode typescript-mode
           markdown-mode rspec-mode python-mode go-mode rust-mode
           emacs-lisp-mode lisp-mode clojure-mode) . yas-minor-mode)
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

(provide 'init-completion)
;;; init-completion.el ends here
