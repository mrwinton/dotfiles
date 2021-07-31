;;; init-windows.el --- windows -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package switch-window
  :demand t
  :config
  (setq-default switch-window-shortcut-style 'alphabet)
  (setq-default switch-window-timeout nil)
  (global-set-key (kbd "C-x o") 'switch-window))

(defun mrwinton/split-window-below-and-switch ()
  "Split the window horizontally, then switch to the new pane."
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))

(defun mrwinton/split-window-right-and-switch ()
  "Split the window vertically, then switch to the new pane."
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))

(global-set-key (kbd "C-x 2") 'mrwinton/split-window-below-and-switch)
(global-set-key (kbd "C-x 3") 'mrwinton/split-window-right-and-switch)

(defun mrwinton/yank-buffer-path (&optional root)
  "Copy the current buffer's path to the kill ring."
  (interactive)
  (if-let (filename (or (buffer-file-name (buffer-base-buffer))
                        (bound-and-true-p list-buffers-directory)))
      (message "Copied path to clipboard: %s"
               (kill-new (abbreviate-file-name
                          (if root
                              (file-relative-name filename root)
                            filename))))
    (error "Couldn't find filename in current buffer")))

(defun mrwinton/yank-buffer-path-relative-to-project ()
  "Copy the current buffer's path to the kill ring."
  (interactive)
  (mrwinton/yank-buffer-path (projectile-project-root)))

(global-set-key (kbd "C-c . y") 'mrwinton/yank-buffer-path)
(global-set-key (kbd "C-c . Y") 'mrwinton/yank-buffer-path-relative-to-project)

(provide 'init-windows)
