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

(use-package undo-tree
  :defer 1
  :bind (:map undo-tree-map
              ("M-/" . undo-tree-redo))
  :custom
  (undo-tree-auto-save-history t)
  (undo-tree-enable-undo-in-region nil)
  (undo-tree-visualizer-diff t)
  :config
  (global-undo-tree-mode))

(use-package ws-butler
  :hook
  (prog-mode . ws-butler-mode))

;; Package `visual-regexp' provides an alternate version of
;; `query-replace' which highlights matches and replacements as you
;; type.
(use-package visual-regexp
  :commands (vr/query-replace)
  :bind (([remap query-replace] . #'vr/query-replace)
         ("M-r" . 'vr/query-replace)))

(use-package expand-region
  :commands (er/expand-region)
  :bind (("C-=" . 'er/expand-region)))

(use-package multiple-cursors
  :commands (mc/mark-next-like-this mc/mark-previous-like-this mc/mark-all-like-this)
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C->" . mc/mark-all-like-this)))

(use-package whole-line-or-region
  :defer t
  :init
  (whole-line-or-region-global-mode))

(use-package move-dup
  :hook (after-init . move-dup-mode)
  :commands (md/move-lines-up md/move-lines-down)
  :bind (([M-up] . md/move-lines-up)
         ([M-down] . md/move-lines-down))
  :config
  (global-move-dup-mode))

(use-package vlf
  :hook (after-init . (lambda () (require 'vlf-setup))))

(use-package dumb-jump
  :custom
  (xref-show-definitions-function #'xref-show-definitions-completing-read)
  :config
  (add-to-list 'xref-backend-functions #'dumb-jump-xref-activate t))

(use-package rainbow-mode)

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

(defun mrwinton/smart-open-line-above ()
  "Insert an empty line above the current line."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

(global-set-key [(control shift return)] 'mrwinton/smart-open-line-above)
(global-set-key (kbd "M-O") 'mrwinton/smart-open-line-above)

;; Join lines whether you're in a region or not.
(defun mrwinton/smart-join-line (beg end)
  "If in a region, join all the lines in it. If not, join the
current line with the next line."
  (interactive "r")
  (if mark-active
      (mrwinton/join-region beg end)
      (mrwinton/top-join-line)))

(defun mrwinton/top-join-line ()
  "Join the current line with the next line."
  (interactive)
  (delete-indentation 1))

(defun mrwinton/join-region (beg end)
  "Join all the lines in the region."
  (interactive "r")
  (if mark-active
      (let ((beg (region-beginning))
            (end (copy-marker (region-end))))
        (goto-char beg)
        (while (< (point) end)
          (join-line 1)))))

(global-set-key (kbd "M-j") 'mrwinton/smart-join-line)

(provide 'init-text)
;;; init-text.el ends here
