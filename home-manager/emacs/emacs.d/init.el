;;; init.el --- Initialize my biggest bike shed -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into a number of
;; other files.

;;; Code:

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Helper function to safely load modules
(defun mrw/safe-require (feature)
  "Safely require FEATURE with error handling."
  (condition-case err
      (require feature)
    (error
     (message "Failed to load %s: %s" feature (error-message-string err)))))

;; Core modules (order matters)
(mrw/safe-require 'init-startup)
(mrw/safe-require 'init-defaults)
(mrw/safe-require 'init-os)
(mrw/safe-require 'init-path)
(mrw/safe-require 'init-utils)  ; Load early - other modules use these functions
(mrw/safe-require 'init-ui)     ; Load early for visual feedback

;; Feature modules
(mrw/safe-require 'init-minibuffer)
(mrw/safe-require 'init-completion)
(mrw/safe-require 'init-text)
(mrw/safe-require 'init-git)
(mrw/safe-require 'init-langs)
(mrw/safe-require 'init-org)
(mrw/safe-require 'init-ai)

;; Keybindings last (after all functions are defined)
(mrw/safe-require 'init-keybindings)

;; Show startup time
(add-hook 'after-init-hook
          (lambda ()
            (let ((elapsed (float-time
                            (time-subtract after-init-time before-init-time))))
              (message "Emacs loaded in %.2fs! Happy hacking ♥" elapsed)))
          t)

;; Extensions
(require 'init-local nil t)

(provide 'init)
;;; init.el ends here
