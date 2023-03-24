;;; init-minibuffer.el --- minibuffer configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package orderless
  :config
  (defun mrw/orderless-without-dispatcher (pattern _index _total)
    "Literal style dispatcher using the equals sign as a prefix.
It matches PATTERN _INDEX and _TOTAL according to how Orderless
parses its input."
    (cond
     ((equal "!" pattern)
      '(orderless-literal . ""))
     ((string-prefix-p "!" pattern)
      `(orderless-without-literal . ,(substring pattern 1)))))

  (defun mrw/orderless-literal-dispatcher (pattern _index _total)
    "Literal style dispatcher using the equals sign as a suffix.
It matches PATTERN _INDEX and _TOTAL according to how Orderless
parses its input."
    (when (string-suffix-p "=" pattern)
      `(orderless-literal . ,(substring pattern 0 -1))))

  (defun mrw/orderless-flex-dispatcher (pattern _index _total)
    "Flex  dispatcher using the tilde suffix.
It matches PATTERN _INDEX and _TOTAL according to how Orderless
parses its input."
    (when (string-suffix-p "~" pattern)
      `(orderless-flex . ,(substring pattern 0 -1))))

  (setq orderless-matching-styles '(orderless-flex
                                    orderless-regexp
                                    orderless-prefixes
                                    orderless-literal))
  (setq orderless-style-dispatchers '(mrw/orderless-without-dispatcher
                                      mrw/orderless-literal-dispatcher
                                      mrw/orderless-flex-dispatcher)))

(use-package minibuffer
  :straight (:type built-in)
  :custom
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (completion-ignore-case t)
  (completion-styles '(substring orderless))
  (completion-category-overrides
   '((file (styles . (basic partial-completion orderless)))))
  (completion-category-defaults nil)
  (completions-group t)
  (completions-group-sort nil)
  (completions-group-format
   (concat
    (propertize "    " 'face 'completions-group-separator)
    (propertize " %s " 'face 'completions-group-title)
    (propertize " " 'face 'completions-group-separator
                'display '(space :align-to right)))))

(use-package vertico
  :straight (vertico :files (:defaults "extensions/*")
                     :includes (vertico-buffer
                                vertico-directory
                                vertico-flat
                                vertico-indexed
                                vertico-mouse
                                vertico-quick
                                vertico-repeat
                                vertico-reverse))
  :hook ((rfn-eshadow-update-overlay-hook . vertico-directory-tidy)
         (after-init . vertico-mode))
  :bind
  (:map vertico-map
        ("RET" . vertico-directory-enter)
        ("DEL" . vertico-directory-delete-char)
        ("M-DEL" . vertico-directory-delete-word))
  :init
  (vertico-mode))

(use-package marginalia
  :after vertico
  :init
  (marginalia-mode))

(use-package embark
  :bind
  ("C-." . embark-act)
  ("M-." . embark-dwim)
  ("C-h B" . embark-bindings)
  (:map minibuffer-local-map
        ("C-c C-o" . embark-export)
        ("C-c C-a" . embark-act)
        ("C-c C-c" . embark-collect-snapshot))
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package consult
  :after projectile
  :bind
  ("C-s" . consult-line)
  ;; C-c bindings (mode-specific-map)
  ("C-c h" . consult-history)
  ("C-c m" . consult-mode-command)
  ("C-c k" . consult-man)
  ;; C-x bindings (ctl-x-map)
  ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
  ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
  ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
  ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
  ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
  ;; Other custom bindings
  ("M-y" . consult-yank-pop)                ;; orig. yank-pop
  ("<help> a" . consult-apropos)            ;; orig. apropos-command
  ;; M-g bindings (goto-map)
  ("M-g e" . consult-compile-error)
  ("M-g g" . consult-goto-line)             ;; orig. goto-line
  ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
  ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
  ("M-g m" . consult-mark)
  ("M-g k" . consult-global-mark)
  ;; M-s bindings (search-map)
  ("M-s d" . consult-find)
  ("M-s D" . consult-locate)
  ("M-s g" . consult-grep)
  ("M-s G" . consult-git-grep)
  ("M-s r" . consult-ripgrep)
  ("M-s l" . consult-line)
  ("M-s L" . consult-line-multi)
  ("M-s m" . consult-multi-occur)
  ("M-s k" . consult-keep-lines)
  ("M-s u" . consult-focus-lines)
  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI. You may want to also
  ;; enable `consult-preview-at-point-mode` in Embark Collect buffers.
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  ;; Replace `completing-read-multiple' with an enhanced version
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (setq consult-narrow-key "<")

  ;;;; 1. project.el (project-roots)
  ;; (setq consult-project-root-function
  ;;       (lambda ()
  ;;         (when-let (project (project-current))
  ;;           (car (project-roots project)))))
  ;;;; 2. projectile.el (projectile-project-root)
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-root-function #'projectile-project-root)
  ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-root-function #'vc-root-dir)
  ;;;; 4. locate-dominating-file
  ;; (setq consult-project-root-function (lambda () (locate-dominating-file "." ".git")))
  )

(use-package consult-flycheck
  :bind
  ("M-g f" . consult-flycheck))

(use-package embark-consult
  :after (embark)
  :demand t)

(provide 'init-minibuffer)
;;; init-minibuffer.el ends here
