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

;; Custom notification function for macOS with sound
(defun mrw/claude-notify (title message)
  "Display a macOS notification with sound."
  (call-process "osascript" nil nil nil
                "-e" (format "display notification \"%s\" with title \"%s\" sound name \"Glass\""
                             message title)))

;; Configure claude-code to use custom notifications
(setq claude-code-notification-function #'mrw/claude-notify)

;; Additional customization for better experience
;; Reduce flickering in Claude buffers on window configuration changes
(add-hook 'claude-code-start-hook
          (lambda ()
            (setq-local eat-minimum-latency 0.033
                        eat-maximum-latency 0.1)
            ;; Reduce line spacing to fix vertical bar gaps
            (setq-local line-spacing 0.1)))

;; Customize cursor type in read-only mode for eat backend
(setq claude-code-eat-read-only-mode-cursor-type '(bar nil nil))

;; Control eat scrollback size for longer conversations
(setq eat-term-scrollback-size 500000)  ; Increase to 500k characters

(provide 'init-ai)
;;; init-ai.el ends here 