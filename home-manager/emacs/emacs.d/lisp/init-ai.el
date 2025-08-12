;;; init-ai.el --- Claude Code AI integration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Install and configure eat terminal backend
(use-package eat
  :ensure t)

;; Install and configure claude-code-ide.el
(use-package claude-code-ide
  :straight (:type git :host github :repo "manzaltu/claude-code-ide.el")
  :bind ("C-c C-'" . claude-code-ide-menu)
  :config
  (claude-code-ide-emacs-tools-setup)
  ;; Configure eat as terminal backend
  (setq claude-code-ide-terminal-backend 'eat))

(provide 'init-ai)
;;; init-ai.el ends here
