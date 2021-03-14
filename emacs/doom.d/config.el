;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Michael Winton"
  user-mail-address "wintonmr@gmail.com"

  ;; doom-theme 'doom-dracula
  ;; doom-theme 'doom-one-light
  ;; doom-theme 'doom-flatwhite
  ;; doom-theme 'doom-tomorrow-day
  doom-theme 'doom-ayu-light
  display-line-numbers-type nil
  doom-font (font-spec :family "Menlo" :size 14 :weight 'normal)

  lsp-enable-file-watchers nil

  projectile-project-search-path '("~/repos/personal/" "~/repos/work/")
  projectile-enable-caching nil

  org-directory "~/org/"
  lsp-solargraph-autoformat t
  require-final-newline t)

;; Update modifier keys on mac
(cond (IS-MAC
       (setq mac-command-modifier      'meta
             mac-option-modifier       'alt
             mac-right-option-modifier 'alt)))

;; Automatically update a buffer when file changes on disk
(global-auto-revert-mode)
(auto-revert-mode)

;; Automatically turn on Mr P's whole line or region behaviour
(whole-line-or-region-global-mode)

;; Turn on word wrapping (almost) everywhere
(+global-word-wrap-mode +1)

;; Highlight the search term in the results
(setq ag-highlight-search t)

;; Configure tabnine's autocompletion
(setq +lsp-company-backends '(company-capf
                              :with company-tabnine
                              :separate))

(after! company
  (setq company-idle-delay 0
        company-show-numbers t
        company-minimum-prefix-length 2))

;; Go fullscreen on startup
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; blinky on
(blink-cursor-mode 1)

;;; :lang org
(setq org-directory "~/Library/Mobile Documents/com~apple~CloudDocs/Documents/10-19 Notes/11 Personal/11.01 org/"
      org-archive-location (concat org-directory ".archive/%s::")
      org-startup-folded 'overview)

;; Utilities

(map! "C->"     #'mc/mark-next-like-this
      "C-<"     #'mc/mark-previous-like-this
      "C-c C-<" #'mc/mark-all-like-this

      "C-."     #'+default/search-project-for-symbol-at-point

      "C-:"     #'avy-goto-char

      "C-c \\"  #'+format/region-or-buffer)

(defun mrw/smart-open-line ()
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current
mode."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(defun mrw/smart-open-line-above ()
  "Insert an empty line above the current line.
Position the cursor at it's beginning, according to the current
mode."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

(global-set-key [(shift return)] 'mrw/smart-open-line)
(global-set-key (kbd "M-o") 'mrw/smart-open-line)
(global-set-key [(control shift return)] 'mrw/smart-open-line-above)
(global-set-key (kbd "M-O") 'mrw/smart-open-line-above)

;; Focus the emacs window in the foreground
(x-focus-frame nil)

;;
;;;; Configuring Doom
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; 'K' over a highlighted symbol for more information.
;; 'gd' to jump to their definition.
