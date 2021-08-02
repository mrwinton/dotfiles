;;; init-completion.el --- completion -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Package `selectrum' is an incremental completion and narrowing
;; framework. Like Ivy and Helm, which it improves on, Selectrum
;; provides a user interface for choosing from a list of options by
;; typing a query to narrow the list, and then selecting one of the
;; remaining candidates. This offers a significant improvement over
;; the default Emacs interface for candidate selection.
(use-package selectrum
  :straight (:host github :repo "raxod502/selectrum")
  :defer t
  :init

  ;; This doesn't actually load Selectrum.
  (selectrum-mode +1))

;; Package `prescient' is a library for intelligent sorting and
;; filtering in various contexts.
(use-package prescient
  :config

  ;; Remember usage statistics across Emacs sessions.
  (prescient-persist-mode +1)

  ;; The default settings seem a little forgetful to me. Let's try
  ;; this out.
  (setq prescient-history-length 1000))

;; Package `selectrum-prescient' provides intelligent sorting and
;; filtering for candidates in Selectrum menus.
(use-package selectrum-prescient
  :straight (:host github :repo "raxod502/prescient.el"
                   :files ("selectrum-prescient.el"))
  :defer 1
  :after selectrum
  :config

  (selectrum-prescient-mode +1))

;; Enable richer annotations using the Marginalia package
(use-package marginalia
  :after (selectrum)
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

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
  (setq completion-styles '(substring))
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-root-function #'projectile-project-root))

(use-package embark
  :after selectrum
  :bind (:map minibuffer-local-map
        ("C-c C-o" . embark-export)
        ("C-c C-a" . embark-act)
        ("C-c C-c" . embark-collect-snapshot))
  :commands (embark-dwim embark-act embark-prefix-help-command)
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; https://github.com/oantolin/embark/wiki/Additional-Configuration
  (setq embark-action-indicator
	      (lambda (map &optional _target)
	        (which-key--show-keymap "Embark" map nil nil 'no-paging)
	        #'which-key--hide-popup-ignore-command)
	      embark-become-indicator embark-action-indicator)

  (defun refresh-selectrum ()
    (setq selectrum--previous-input-string nil))

  (add-hook 'embark-pre-action-hook #'refresh-selectrum) 

  (defun shrink-selectrum ()
    (when (eq embark-collect--kind :live)
      (with-selected-window (active-completion-window)
	      (setq-local selectrum-num-candidates-displayed 1)
	      (setq-local selectrum-display-style
		                '(horizontal :before-candidates "[" :after-candidates "]"
				                         :more-candidates "" :candidates-separator "")))))

  (add-hook 'embark-collect-mode-hook #'shrink-selectrum))

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
