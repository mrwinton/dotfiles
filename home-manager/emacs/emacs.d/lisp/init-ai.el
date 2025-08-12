;;; init-ai.el --- Claude Code AI integration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Install and configure vterm terminal backend
(use-package vterm
  :ensure t
  :config
  ;; Better performance for Claude Code IDE
  (setq vterm-max-scrollback 5000)
  (setq vterm-kill-buffer-on-exit t))

;; Install and configure claude-code-ide.el
(use-package claude-code-ide
  :straight (:type git :host github :repo "manzaltu/claude-code-ide.el")
  :config
  (claude-code-ide-emacs-tools-setup)
  ;; Configure vterm as terminal backend
  (setq claude-code-ide-terminal-backend 'vterm)
  ;; Anti-flickering optimizations for vterm
  (setq claude-code-ide-vterm-anti-flicker t)
  (setq claude-code-ide-vterm-render-delay 0.005))

(provide 'init-ai)
;;; init-ai.el ends here
