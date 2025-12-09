;;; init-utils.el --- utils configuration  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun mrw/rg ()
  "Allows you to select a folder to ripgrep."
  (interactive)
  (let ((current-prefix-arg 4)) ;; emulate C-u
    (call-interactively 'consult-ripgrep)))

(defun mrw/deft ()
  "Helper to call deft and then fix things so that it is nice and works"
  (interactive)
  (if (fboundp 'deft)
      (progn
        (deft)
        ;; Hungry delete wrecks deft's DEL override
        (when (fboundp 'hungry-delete-mode)
          (hungry-delete-mode -1)))
    (error "Deft is not available")))

(defun mrw/switch-to-scratch-buffer ()
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun mrw/kill-other-buffers (&optional arg)
  "Kill all other buffers.
If the universal prefix argument is used then will the windows too."
  (interactive "P")
  (when (yes-or-no-p (format "Killing all buffers except \"%s\"? "
                             (buffer-name)))
    (mapc 'kill-buffer (delq (current-buffer) (buffer-list)))
    (when (equal '(4) arg) (delete-other-windows))
    (message "Buffers deleted!")))

(defun mrw/new-empty-buffer ()
  "Create a new buffer called untitled(<n>)"
  (interactive)
  (let ((newbuf (generate-new-buffer-name "*scratch*")))
    (switch-to-buffer newbuf)))

(defun mrw/split-window-below-and-switch ()
  "Split the window horizontally, then switch to the new pane."
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))

(defun mrw/split-window-right-and-switch ()
  "Split the window vertically, then switch to the new pane."
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))

(defun mrw/toggle-maximize-buffer ()
  "Maximize buffer"
  (interactive)
  (if (and (= 1 (length (window-list)))
           (assoc ?_ register-alist))
      (jump-to-register ?_)
    (progn
      (window-configuration-to-register ?_)
      (delete-other-windows))))

(defun mrw/yank-buffer-path (&optional root)
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

(defun mrw/yank-buffer-path-relative-to-project ()
  "Copy the current buffer's path to the kill ring."
  (interactive)
  (if (and (featurep 'projectile) (projectile-project-root))
      (mrw/yank-buffer-path (projectile-project-root))
    (error "Not in a projectile project")))

(defun mrw/open-buffer-file-mac ()
  "Open current buffer file using Mac `open' command."
  (interactive)
  (shell-command (concat "open " (buffer-file-name))))

(defun mrw/completion-in-region (&rest args)
    (apply (if (and (boundp 'vertico-mode) vertico-mode)
                   #'consult-completion-in-region
               #'completion--in-region)
           args))

(defun mrw/crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))

(defun mrw/toggle-theme ()
  "Toggle between light and dark themes."
  (interactive)
  (if (eq (car custom-enabled-themes) mrw/dark-theme)
      (modus-themes-load-theme mrw/light-theme)
    (modus-themes-load-theme mrw/dark-theme)))

(provide 'init-utils)
;;; init-utils.el ends here
