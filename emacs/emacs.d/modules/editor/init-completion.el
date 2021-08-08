;;; init-completion.el --- completion -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package consult
  :after selectrum
  :preface
  (defun mrwinton/consult-line-symbol-at-point ()
    (interactive)
    (consult-line (thing-at-point 'symbol)))
  (defun mrwinton/consult-grep-at-point (&optional dir initial)
    (interactive (list prefix-arg (when-let ((s (symbol-at-point)))
                                    (symbol-name s))))
    (let ((command (cond ((executable-find "rg") #'consult-ripgrep)
                         ((executable-find "git" #'consult-git-grep))
                         (t #'consult-grep))))
      (funcall command dir initial)))
  :bind
  ("C-c b" . consult-buffer)
  ("C-c f" . consult-find)
  ("C-c k" . consult-man)
  ("C-s"   . consult-line)
  ("C-c r" . consult-ripgrep)
  ("C-?"   . mrwinton/consult-line-symbol-at-point)
  ("M-?"   . mrwinton/consult-grep-at-point)
  ("M-y"   . consult-yank-pop)
  ("M-g g" . consult-line)
  :custom
  (consult-async-input-debounce 0.1)
  (consult-async-input-throttle 0.2)
  (consult-async-refresh-delay  0.15)
  (consult-find-command "fd -HLp -E .git -c never -t f ARG OPTS")
  (consult-line-numbers-widen t)
  (consult-narrow-key "<")
  :init (advice-add #'completing-read-multiple
                    :override #'consult-completing-read-multiple)
  :config
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-root-function #'projectile-project-root))

(use-package hotfuzz
  :defer 1
  :straight (hotfuzz :type git :host github :repo "axelf4/hotfuzz")
  :config
  (hotfuzz-selectrum-mode +1))

(use-package selectrum
  :config
  (selectrum-mode +1)
  :bind (:map selectrum-minibuffer-map
              ("C-j" . selectrum-next-candidate)
              ("C-k" . selectrum-previous-candidate)))

(use-package selectrum-prescient
  :defer 1
  :config
  ;; to make sorting and filtering more intelligent
  (selectrum-prescient-mode +1)

  ;; to save your command history on disk, so the sorting gets more
  ;; intelligent over time
  (prescient-persist-mode +1))

(use-package marginalia
  :after selectrum
  :config
  (add-to-list 'marginalia-prompt-categories '("Find file" . file))
  (marginalia-mode))

(use-package embark
  :after selectrum
  :bind (("C-." . embark-act)
         :map minibuffer-local-map
         ("C-c C-o" . embark-export)
         ("C-c C-a" . embark-act)
         ("C-c C-c" . embark-collect-snapshot))
  :commands (embark-dwim embark-act embark-prefix-help-command)
  :config
  (setq prefix-help-command #'embark-prefix-help-command)
  (setq embark-action-indicator
        (lambda (map _target)
          (which-key--show-keymap "Embark" map nil nil 'no-paging)
          #'which-key--hide-popup-ignore-command)
        embark-become-indicator embark-action-indicator)
  (add-hook 'embark-collect-post-revert-hook
            (defun resize-embark-collect-window (&rest _)
              (when (memq embark-collect--kind '(:live :completions))
                (fit-window-to-buffer (get-buffer-window)
                                      (floor (frame-height) 2) 1)))))

(use-package embark-consult
  :after
  (embark consult)
  :hook
  (embark-collect-mode . embark-consult-preview-minor-mode))

;; Writable grep buffer
(use-package wgrep
  :defer 1
  :custom
  (wgrep-auto-save-buffer t)
  (wgrep-change-readonly-file t))

;;; Finding files

;; Follow symlinks when opening files. This has the concrete impact,
;; for instance, that when you edit init.el with M-P e e i and then
;; later do C-x C-f, you will be in the Radian repository instead of
;; your home directory.
(setq find-file-visit-truename t)

;; Disable the warning "X and Y are the same file" which normally
;; appears when you visit a symlinked file by the same name. (Doing
;; this isn't dangerous, as it will just redirect you to the existing
;; buffer.)
(setq find-file-suppress-same-file-warnings t)

(provide 'init-completion)
