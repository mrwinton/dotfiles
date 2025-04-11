;;; init-completion.el --- completion configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package corfu
  :hook (after-init . global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.3)
  (corfu-auto-prefix 3)
  (corfu-quit-at-boundary t)     ;; Automatically quit at word boundary
  (corfu-quit-no-match t)        ;; Automatically quit if there is no match
  (corfu-echo-documentation nil) ;; Do not show documentation in the echo area
  (corfu-cycle t))

(use-package cape
  :after corfu
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand)))

(use-package emacs
  :straight (:type built-in)
  :init
  ;; TAB cycle if there are only few candidates
  ;; (setq completion-cycle-threshold 3)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))

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

(provide 'init-completion)
;;; init-completion.el ends here
