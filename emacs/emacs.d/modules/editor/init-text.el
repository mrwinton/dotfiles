;;; init-text.el --- text -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package crux
  :bind
  ("C-^" . crux-top-join-line)
  ("C-a" . crux-move-beginning-of-line)
  ("C-o" . crux-smart-open-line-above)
  ("M-o" . crux-smart-open-line)
  ("C-<BACKSPACE>" . crux-kill-line-backwards)
  ("C-<DEL>" . crux-kill-line-forwards)
  ("C-c c" . crux-create-scratch-buffer)
  ("C-c d" . crux-duplicate-current-line-or-region)
  ("C-c M-d" . crux-duplicate-and-comment-current-line-or-region)
  ("C-c D" . crux-delete-file-and-buffer)
  ("C-c r" . crux-rename-buffer-and-file)
  ("C-c t" . crux-visit-term-buffer)
  ("C-h RET" . crux-find-user-init-file)
  ("C-x x e" . crux-open-with)
  ("C-x 7" . crux-swap-windows))

(use-package editorconfig
  :defer 1
  :config
  (editorconfig-mode 1))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package smartparens
  :defer 1
  :config
  ;; Load the default pair definitions
  (require 'smartparens-config)

  ;; Enable smartparens functionality in all buffers
  (smartparens-global-mode +1)

  ;; Highlight matching delimiters
  (show-smartparens-global-mode +1))

(use-package undo-tree
  :defer 1
  :bind (:map undo-tree-map
              ("M-/" . undo-tree-redo))
  :config
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-enable-undo-in-region nil)
  (setq undo-tree-visualizer-diff t)

  (global-undo-tree-mode))

(use-package ws-butler
  :defer 1
  :config
  (add-hook 'prog-mode-hook #'ws-butler-mode))

;; Package `visual-regexp' provides an alternate version of
;; `query-replace' which highlights matches and replacements as you
;; type.
(use-package visual-regexp
  :defer 1
  :bind (("C-c q" . #'vr/query-replace)))

(use-package expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

(use-package multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(use-package whole-line-or-region
  :init
  (whole-line-or-region-global-mode))

(use-package move-dup
  :defer 1
  :bind (([M-up] . md/move-lines-up)
         ([M-down] . md/move-lines-down))
  :config
  (global-move-dup-mode))

;; Open large files fast
(use-package vlf
  :defer 1)

(use-package dumb-jump
  :config
  (add-to-list 'xref-backend-functions #'dumb-jump-xref-activate t)
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read))

(defun mrwinton/smart-open-line-above ()
  "Insert an empty line above the current line.
                              Position the cursor at it's beginning, according to the current
                              mode."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

(global-set-key [(control shift return)] 'mrwinton/smart-open-line-above)
(global-set-key (kbd "M-O") 'mrwinton/smart-open-line-above)

;; Easily adjust the font size in all frames
(use-package default-text-scale
  :hook (after-init . default-text-scale-mode)
  :bind (:map default-text-scale-mode-map
              ("s-=" . default-text-scale-increase)
              ("s--" . default-text-scale-decrease)
              ("s-0" . default-text-scale-reset)
              ("C-s-=" . default-text-scale-increase)
              ("C-s--" . default-text-scale-decrease)
              ("C-s-0" . default-text-scale-reset)))

(provide 'init-text)
