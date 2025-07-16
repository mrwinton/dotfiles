;;; init-ai.el --- Claude Code AI integration -*- lexical-binding: t; -*-
;;; Commentary:
;; Emacs integration for Claude Code CLI via claude-code.el
;;; Code:

;; Install and configure eat terminal backend
(use-package eat
  :ensure t)

;; Install and configure claude-code.el
(use-package claude-code
  :straight (:host github :repo "stevemolitor/claude-code.el")
  :config
  (claude-code-mode)
  :bind-keymap ("C-c c" . claude-code-command-map))

;; Font configuration for Unicode support in Claude buffers
(custom-set-faces
 '(claude-code-repl-face ((t (:family "JuliaMono")))))

(provide 'init-ai)
;;; init-ai.el ends here 