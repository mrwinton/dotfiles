;;; init-minibuffer.el --- minibuffer configuration  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package orderless
  :config
  ;; orderless-initialism: 'abc' matches 'a<ny>b<any>c' at word boundaries
  ;; orderless-flex: 'abc' matches 'a.*b.*c' (more flexible)
  ;; orderless-prefixes: 're-re' matches 'query-replace-regexp'
  (setq orderless-matching-styles '(orderless-flex
                                    orderless-initialism
                                    orderless-regexp
                                    orderless-prefixes
                                    orderless-literal))

  ;; Built-in affix dispatcher for query syntax (prefix or suffix):
  ;;   ! or suffix !  = negate pattern (orderless-not)
  ;;   ,              = initialism
  ;;   =              = literal match
  ;;   ~              = flex match
  ;;   ^              = literal prefix
  ;;   %              = char-fold (ignore diacritics)
  ;;   &              = match against annotation
  (setq orderless-style-dispatchers '(orderless-affix-dispatch)))

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
         (after-init . vertico-mode)))

(use-package marginalia
  :after vertico
  :init
  (marginalia-mode))

(use-package embark
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

(use-package consult-flycheck)

(use-package consult-dir
  :commands (consult-dir consult-dir-jump-file))

(use-package embark-consult
  :after (embark)
  :demand t)

(provide 'init-minibuffer)
;;; init-minibuffer.el ends here
