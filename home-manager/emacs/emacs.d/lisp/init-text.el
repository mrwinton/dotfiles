;;; init-text.el --- text configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package dired
  :straight (:type built-in)
  :after exec-path-from-shell
  :custom
  (dired-listing-switches "-ahl --group-directories-first")
  (dired-dwim-target t)
  (dired-use-ls-dired t)
  (dired-recursive-deletes 'always)
  (dired-recursive-copies 'always)
  (dired-clean-up-buffers-too t)
  (insert-directory-program (executable-find "ls")))

(use-package crux
  :init
  (define-prefix-command 'mrw-crux-map nil "crux-")
  :config
  (crux-with-region-or-buffer indent-region)
  (crux-with-region-or-buffer untabify)
  (crux-with-region-or-point-to-eol kill-ring-save)
  (defalias 'rename-file-and-buffer #'crux-rename-file-and-buffer)
  )

(use-package editorconfig
  :hook ((prog-mode . editorconfig-mode)))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package smartparens
  :hook
  (prog-mode . smartparens-mode)
  :config
  ;; Load the default pair definitions
  (require 'smartparens-config)

  ;; Enable smartparens functionality in all buffers
  (smartparens-global-mode +1)

  ;; Highlight matching delimiters
  (show-smartparens-global-mode +1)

  ;; The default is 100, because smartparen's scans are relatively expensive
  ;; (especially with large pair lists for some modes), we reduce it, as a
  ;; better compromise between performance and accuracy.
  (setq sp-max-prefix-length 25)

  ;; No pair has any business being longer than 4 characters; if they must, set
  ;; it buffer-locally. It's less work for smartparens.
  (setq sp-max-pair-length 4)

  ;; Silence some harmless but annoying echo-area spam
  (dolist (key '(:unmatched-expression :no-matching-tag))
    (setf (alist-get key sp-message-alist) nil))

  ;; Fix pairing in web-mode
  (sp-with-modes '(web-mode)
    (sp-local-pair "%" "%"
                   :unless '(sp-in-string-p)
                   :post-handlers '(((lambda (&rest _ignored)
                                       (just-one-space)
                                       (save-excursion (insert " ")))
                                     "SPC" "=" "#")))
    (sp-local-tag "%" "<% "  " %>")
    (sp-local-tag "=" "<%= " " %>")
    (sp-local-tag "#" "<%# " " %>")))

(use-package vundo
  :commands (vundo)
  :custom
  (vundo-glyph-alist vundo-unicode-symbols)
  (vundo-compact-display nil))

(use-package undo-hl
  :straight (undo-hl :type git :host github :repo "casouri/undo-hl")
  :hook
  (prog-mode . undo-hl-mode)
  :custom
  (undo-hl-minimum-edit-size 10)
  :config
  (add-to-list 'undo-hl-undo-commands 'vundo-forward)
  (add-to-list 'undo-hl-undo-commands 'vundo-backward))

(use-package ws-butler
  :hook
  (prog-mode . ws-butler-mode)
  (org-mode . ws-butler-mode))

;; Package `visual-regexp' provides an alternate version of
;; `query-replace' which highlights matches and replacements as you
;; type.
(use-package visual-regexp
  :commands (vr/query-replace))

(use-package expand-region
  :commands (er/expand-region))

(use-package multiple-cursors
  :commands (mc/mark-next-like-this mc/mark-previous-like-this mc/mark-all-like-this))

(use-package whole-line-or-region
  :defer t
  :init
  (whole-line-or-region-global-mode))

(use-package move-dup
  :commands (md/move-lines-up md/move-lines-down)
  :config
  (global-move-dup-mode))

(use-package vlf
  :hook (after-init . (lambda () (require 'vlf-setup))))

(use-package rainbow-mode)

(use-package super-save
  :hook (after-init . super-save-mode)
  :custom
  (super-save-remote-files nil)
  (super-save-auto-save-when-idle t))

(use-package tree-sitter
  :hook
  (after-init . global-tree-sitter-mode)
  (tree-sitter-after-on . tree-sitter-hl-mode)
  :config
  (add-to-list 'tree-sitter-major-mode-language-alist '(enh-ruby-mode . ruby)))

(use-package tree-sitter-langs
  :commands global-tree-sitter-mode)

(use-package flycheck
  :commands (flycheck-mode flycheck-list-errors flycheck-buffer flycheck-add-next-checker)
  :hook (prog-mode . flycheck-mode)
  :custom
  (flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)
  (flycheck-check-syntax-automatically '(save idle-change new-line mode-enabled))
  (flycheck-indication-mode 'right-fringe)
  (flycheck-highlighting-mode nil)
  :config
  (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
    [16 48 112 240 112 48 16] nil nil 'center)
  (global-flycheck-mode))

(use-package flycheck-posframe
  :hook (flycheck-mode . flycheck-posframe-mode)
  :config
  (flycheck-posframe-configure-pretty-defaults))

(use-package format-all
  :commands (format-all-buffer format-all-mode)
  :hook (prog-mode . format-all-mode))

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

(use-package jinx
  :after exec-path-from-shell
  :hook (after-init . global-jinx-mode)
  :config (setq jinx-languages "en_US en_GB sv_SE"))

(use-package flyspell
  :diminish flyspell-mode
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode)))

(use-package flyspell-lazy
  :after flyspell
  :commands (flyspell-lazy-mode)
  :defines (flyspell-lazy-idle-seconds
            flyspell-lazy-window-idle-seconds)
  :config
  (setq flyspell-lazy-idle-seconds 1
        flyspell-lazy-window-idle-seconds 3)
  (flyspell-lazy-mode +1))

(use-package flyspell-correct
  :after flyspell)

(use-package flyspell-correct-popup
  :after flyspell-correct)

(use-package projectile
  :commands (projectile-mode projectile-switch-project)
  :custom
  (projectile-project-search-path '(("~/src" . 3)))
  (projectile-indexing-method 'alien)

  ;; Use Selectrum (via `completing-read') for Projectile instead of IDO.
  (projectile-completion-system 'default)

  ;; When switching projects, give the option to choose what to do. This is a
  ;; way better interface than having to remember ahead of time to use a prefix
  ;; argument on `projectile-switch-project' (because, and please be honest
  ;; here, when was the last time you actually remembered to do that?).
  (projectile-switch-project-action #'projectile-dired)
  :config
  (projectile-mode +1))

;; Writable grep buffer
(use-package wgrep
  :commands wgrep-change-to-wgrep-mode
  :custom
  (wgrep-auto-save-buffer t)
  (wgrep-change-readonly-file t))

(provide 'init-text)
;;; init-text.el ends here
