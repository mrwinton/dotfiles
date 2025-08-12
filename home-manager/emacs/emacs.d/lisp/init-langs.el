;;; init-langs.el --- langs configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package eglot
  :hook (((ruby-mode enh-ruby-mode) . eglot-ensure)
         ((js2-mode typescript-ts-mode js-mode) . eglot-ensure))
  :custom
  (eglot-autoshutdown t)
  (eglot-sync-connect nil)
  (eglot-autoreconnect nil)
  (eglot-send-changes-idle-time 0.5)
  :config
  ;; Tab completion integration
  (setopt tab-always-indent 'complete)
  (setopt completion-styles '(basic initials substring))

  ;; Better ElDoc
  (setq eldoc-echo-area-use-multiline-p nil)
  ;; Ruby language server
  (add-to-list 'eglot-server-programs '(enh-ruby-mode "solargraph" "socket" "--port" :autoport))

  ;; JavaScript/TypeScript language servers
  (add-to-list 'eglot-server-programs
               '(js-mode "typescript-language-server" "--stdio"))
  (add-to-list 'eglot-server-programs
               '(js2-mode "typescript-language-server" "--stdio"))
  (add-to-list 'eglot-server-programs
               '(typescript-ts-mode "typescript-language-server" "--stdio"))
  (add-to-list 'eglot-server-programs
               '(tsx-ts-mode "typescript-language-server" "--stdio"))

  ;; Herb language server for ERB templates
  ;; Install with: npm install -g @herb-tools/language-server
  (add-to-list 'eglot-server-programs
               '(web-mode "herb-language-server" "--stdio"))

  (defun mrw/eglot-eldoc-message-function (fmt &rest args)
    "Limit `eldoc-mode' strings to one line.
Eglot doesn't heed to `eldoc-echo-area-use-multiline-p'."
    (if-let ((str (and (stringp fmt) (apply #'format fmt args)))
             (line (car (split-string str "\n" t)))
             (limit (min (length line) (1- (frame-width)))))
        (eldoc-minibuffer-message (substring line 0 limit))
      (eldoc-minibuffer-message fmt args)))

  (defun mrw/eglot--managed-mode-hook ()
    "Customizations for `eglot--managed-mode'."
    ;; Setup `eldoc-mode'.
    (eglot--setq-saving eldoc-message-function
                        #'mrw/eglot-eldoc-message-function))

  (add-hook 'eglot--managed-mode-hook #'mrw/eglot--managed-mode-hook))

;; Enhanced CSS support
(use-package css-mode
  :mode (("\\.css\\'" . css-mode)))

(use-package scss-mode
  :mode (("\\.scss\\'" . scss-mode))
  :custom
  (scss-compile-at-save nil))

(use-package less-mode
  :mode (("\\.less\\'" . less-mode)))

;; Modern JavaScript/TypeScript support
(use-package js2-mode
  :mode (("\\.js\\'" . js2-mode)
         ("\\.jsx\\'" . js2-mode))
  :custom
  (js2-auto-indent-p t)
  (js2-enter-indents-newline t)
  (js2-indent-on-enter-key t)
  (js2-global-externs '("module" "require" "process" "console" "JSON" "Buffer"))
  (js2-highlight-level 3)
  (js2-mode-show-parse-errors nil)
  (js2-mode-show-strict-warnings nil))

(use-package typescript-ts-mode
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode)))

;; JSON support with formatting
(use-package json-mode
  :mode (("\\.json\\'" . json-mode)
         ("\\.jsonc\\'" . json-mode))
)

(use-package json-reformat
  :after json-mode
  :commands (json-reformat-region))

;; Web development support
(use-package web-mode
  :mode ("\\.html\\'"
         "\\.phtml\\'"
         "\\.tpl\\.php\\'"
         "\\.[agj]sp\\'"
         "\\.as[cp]x\\'"
         "\\.erb\\'"
         "\\.mustache\\'"
         "\\.djhtml\\'"
         "\\.vue\\'"
         "\\.svelte\\'")
  :custom
  (web-mode-enable-auto-quoting t)
  (web-mode-enable-auto-pairing nil)
  (web-mode-enable-auto-closing t)
  (web-mode-auto-close-style 2)
  (web-mode-enable-auto-indentation t)
  (web-mode-enable-current-element-highlight t)
  (web-mode-enable-current-column-highlight t))

;; GraphQL support
(use-package graphql-mode
  :mode (("\\.gql\\'" . graphql-mode)
         ("\\.graphql\\'" . graphql-mode)))

;; Shell script support
(use-package sh-script
  :straight (:type built-in)
  :mode (("\\.zsh\\'" . sh-mode)
         ("\\.zshrc\\'" . sh-mode)
         ("\\.zshenv\\'" . sh-mode)
         ("\\.zshprofile\\'" . sh-mode)
         ("\\.bash\\'" . sh-mode)
         ("\\.bashrc\\'" . sh-mode)
         ("\\.bashenv\\'" . sh-mode)
         ("\\.bashprofile\\'" . sh-mode))
)

;; Lua support
(use-package lua-mode
  :mode (("\\.lua\\'" . lua-mode)))

;; Markdown support
(use-package markdown-mode
  :mode ("/README\\(?:\\.md\\)?\\'" . gfm-mode)
  :custom
  (markdown-enable-math t)
  (markdown-enable-wiki-links t)
  (markdown-italic-underscore t)
  (markdown-asymmetric-header t)
  (markdown-fontify-code-blocks-natively t)
  (markdown-gfm-additional-languages '("sh" "javascript" "typescript" "css" "scss"))
  (markdown-make-gfm-checkboxes-buttons t))

;; Nix support
(use-package nix-mode
  :interpreter ("\\(?:cached-\\)?nix-shell" . +nix-shell-init-mode)
  :mode "\\.nix\\'")

(use-package nixpkgs-fmt
  :hook (nix-mode . nixpkgs-fmt-on-save-mode))

;; Ruby support
(use-package rspec-mode
  :after ruby-mode
  :custom
  (rspec-spec-command "bundle exec rspec")
  (rspec-use-bundler-when-possible nil)
  (rspec-use-spring-when-possible nil)
  (rspec-use-opts-file-when-available nil)
  (rspec-command-options "--color --format documentation"))

(use-package ruby-tools
  :hook ((ruby-mode ruby-ts-mode) . ruby-tools-mode))

;; Scala support
(use-package scala-mode
  :interpreter
  ("scala" . scala-mode))

;; YAML support
(use-package yaml-mode
  :mode ("\\.yml\\'" "\\.yaml\\'" "\\.yaml.erb\\'"))

(use-package yaml-tomato
  :commands
  (yaml-tomato-show-current-path yaml-tomato-copy))

;; Tree-sitter support for better syntax highlighting
(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  ;; Set up language sources
  (setq treesit-language-source-alist
        '((typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (yaml "https://github.com/tree-sitter/tree-sitter-yaml")
          (ruby "https://github.com/tree-sitter/tree-sitter-ruby")))

  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(provide 'init-langs)
;;; init-langs.el ends here
