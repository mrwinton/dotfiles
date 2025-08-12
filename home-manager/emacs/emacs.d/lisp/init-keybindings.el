;;; init-keybindings.el --- keybindings configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package which-key
  :defer 1
  :config
  (which-key-mode)
  (which-key-setup-minibuffer)
  (setq which-key-idle-delay 0.3)
  (setq which-key-prefix-prefix "◉ ")
  (setq which-key-sort-order 'which-key-key-order-alpha
        which-key-min-display-lines 3
        which-key-max-display-columns nil))

(use-package helpful
  :commands (helpful-callable helpful-key helpful-variable helpful-command helpful-at-point))

(use-package general
  :demand t
  :config
  (general-define-key
   :prefix "C-c"

   ;; Top level functions
   "/" '(mrw/rg :which-key "ripgrep")
   ";" '(mrw/deft :which-key "deft")
   ":" '(project-find-file :which-key "p-find file")
   "." '(find-file :which-key "find file")
   "," '(consult-recent-file :which-key "recent files")
   "TAB" '(switch-to-prev-buffer :which-key "previous buffer")
   "SPC" '(execute-extended-command :which-key "M-x")
   "q" '(save-buffers-kill-terminal :which-key "quit emacs")
   "r" '(jump-to-register :which-key "registers")
   "c" 'org-capture
   "2" '(mrw/split-window-below-and-switch :which-key "split window below")
   "3" '(mrw/split-window-right-and-switch :which-key "split window right")

   ;; Applications
   "a" '(nil :which-key "applications")
   "ao" '(org-agenda :which-key "org-agenda")
   "aC" '(calc :which-key "calc")
   "ac" '(org-capture :which-key "org-capture")
   "ad" '(dired :which-key "dired")

   ;; Buffers
   "b" '(nil :which-key "buffer")
   "bb" '(consult-buffer :which-key "switch buffers")
   "bd" '(kill-buffer :which-key "delete buffer")
   "bs" '(mrw/switch-to-scratch-buffer :which-key "scratch buffer")
   "bm" '(mrw/kill-other-buffers :which-key "kill other buffers")
   "bi" '(clone-indirect-buffer  :which-key "indirect buffer")
   "br" '(revert-buffer :which-key "revert buffer")
   "by" '(mrw/yank-buffer-path :which-key "yank buffer path")
   "bY" '(mrw/yank-buffer-path-relative-to-project :which-key "yank relative buffer path")

   ;; Files
   "f" '(nil :which-key "files")
   "fb" '(consult-bookmark :which-key "bookmarks")
   "ff" '(find-file :which-key "find file")
   "fn" '(mrw/new-empty-buffer :which-key "new file")
   "fr" '(consult-recent-file :which-key "recent files")
   "fR" '(rename-file :which-key "rename file")
   "fs" '(save-buffer :which-key "save buffer")
   "fo" '(reveal-in-osx-finder :which-key "reveal in finder")
   "fO" '(mrw/open-buffer-file-mac :which-key "open buffer file")

   ;; Help/emacs
   "h" '(nil :which-key "help/emacs")
   "hv" '(describe-variable :which-key "des. variable")
   "hb" '(describe-bindings :which-key "des. bindings")
   "hM" '(describe-mode :which-key "des. mode")
   "hf" '(describe-function :which-key "des. func")
   "hF" '(describe-face :which-key "des. face")
   "hk" '(helpful-key :which-key "des. key")
   "hx" '(helpful-command :which-key "des. command")

   ;; Text
   "x" '(nil :which-key "text")
   "xr" '(vr/query-replace :which-key "replace-regexp")
   "xs" '(yas-insert-snippet :which-key "insert yasnippet")
   "xf" '(flush-lines :which-key "flush-lines")
   "xj" '(crux-top-join-line :which-key "join lines")
   "xw" '(whitespace-cleanup :which-key "cleanup whitespace")
   "xa" '(mc/mark-all-like-this :which-key "mark all like this")
   "xn" '(mc/mark-next-like-this :which-key "mark next like this")
   "xp" '(mc/mark-previous-like-this :which-key "mark previous like this")


   ;; Toggles
   "t" '(nil :which-key "toggles")
   "tl" '(toggle-truncate-lines :which-key "truncate lines")
   "tv" '(visual-line-mode :which-key "visual line mode")
   "tn" '(display-line-numbers-mode :which-key "display line numbers")
   "ta" '(mixed-pitch-mode :which-key "variable pitch mode")
   "ty" '(consult-theme :which-key "load theme")
   "tR" '(read-only-mode :which-key "read only mode")
   "tI" '(toggle-input-method :which-key "toggle input method")
   "tr" '(display-fill-column-indicator-mode :which-key "fill column indicator")
   "tt" '(mrw/toggle-theme :which-key "toggle theme")

   ;; Windows
   "w" '(nil :which-key "window")
   "wm" '(mrw/toggle-maximize-buffer :which-key "maximize buffer")
   "w2" '(mrw/split-window-below-and-switch :which-key "split below")
   "w3" '(mrw/split-window-right-and-switch :which-key "split right")
   "wz" '(text-scale-adjust :which-key "text zoom")

   ;; Environment
   "e" '(:keymap envrc-command-map :package envrc :which-key "envrc")

   ;; Git
   "g" '(nil :which-key "git")
   "gb" '(magit-blame :which-key "magit blame")
   "gg" '(magit-status :which-key "magit status")
   "gl" '(git-link :which-key "git link")
   "gt" '(git-timemachine :which-key "git timemachine")
   ))

;; Additional keybindings from text editing packages
(general-define-key
 ;; Global keybindings
 "C-a" 'crux-move-beginning-of-line
 "C-o" 'crux-smart-open-line-above
 "M-o" 'crux-smart-open-line
 "C-x 4 t" 'crux-transpose-windows
 "C-x K" 'crux-kill-other-buffers
 "C-k" 'crux-smart-kill-line
 "C-x u" 'vundo
 "M-r" 'vr/query-replace
 "C-=" 'er/expand-region
 "C-;" 'flyspell-correct-wrapper
 [remap ispell-word] 'jinx-correct
 "M-m" 'mrw-crux-map
 [M-up] 'md/move-lines-up
 [M-down] 'md/move-lines-down)

;; Crux prefix map keybindings
(general-define-key
 :keymaps 'mrw-crux-map
 "w" '(crux-view-url :which-key "Open a new buffer containing the contents of URL.")
 "o" '(crux-open-with :which-key "Open visited file in default external program.")
 "e" '(crux-sudo-edit :which-key "Edit currently visited file as root.")
 "i" '(crux-insert-date :which-key "Insert a timestamp according to locale's date and time format.")
 "t" '(crux-transpose-windows :which-key "Transpose the buffers shown in two windows.")
 "j" '(crux-top-join-line :which-key "Join the current line with the line beneath it.")
 "u" '(upcase-dwim :which-key "upcase region if a region is active or word at point.")
 "d" '(downcase-dwim :which-key "downcase region if a region is active or word at point.")
 "c" '(capitalize-dwim :which-key "capitalize region if a region is active or word at point.")
 "r" '(crux-recompile-init :which-key "Byte-compile all your dotfiles again.")
 "k" '(crux-smart-kill-line :which-key "Kill to the end of the line and kill whole line on the next call.")
 "M-k" '(crux-kill-line-backwards :which-key "Kill line backwards and adjust the indentation.")
 "a" '(crux-move-beginning-of-line :which-key "Move point back to indentation/beginning (toggle) of line.")
 "s" '(crux-ispell-word-then-abbrev :which-key "Call `ispell-word', then create an abbrev for it."))

;; Wgrep keybindings
(general-define-key
 :keymaps 'grep-mode-map
 "C-c C-q" 'wgrep-change-to-wgrep-mode
 "w" 'wgrep-change-to-wgrep-mode)

;; Projectile keymap
(general-define-key
 :prefix "C-c p"
 "" '(:keymap projectile-command-map :package projectile :which-key "projectile"))

;; Claude Code AI keybindings
(general-define-key
 "C-c C-'" 'claude-code-ide-menu)

;; Dabbrev keybindings (from init-completion.el)
(general-define-key
 "M-/" 'dabbrev-completion
 "C-M-/" 'dabbrev-expand)

;; Git keybindings
(general-define-key
 "C-x g" 'magit-status
 "C-c g b" 'magit-blame
 "C-c g t" 'git-timemachine
 "C-c g l" 'git-link)

;; Minibuffer and Consult keybindings
(general-define-key
 ;; Vertico keybindings
 :keymaps 'vertico-map
 "RET" 'vertico-directory-enter
 "DEL" 'vertico-directory-delete-char
 "M-DEL" 'vertico-directory-delete-word)

(general-define-key
 ;; Embark keybindings
 "C-." 'embark-act
 "M-." 'embark-dwim
 "C-h B" 'embark-bindings
 :keymaps 'minibuffer-local-map
 "C-c C-o" 'embark-export
 "C-c C-a" 'embark-act
 "C-c C-c" 'embark-collect-snapshot)

(general-define-key
 ;; Consult keybindings
 "C-s" 'consult-line
 "C-c h" 'consult-history
 "C-c m" 'consult-mode-command
 "C-c k" 'consult-man
 "C-x M-:" 'consult-complex-command
 "C-x b" 'consult-buffer
 "C-x 4 b" 'consult-buffer-other-window
 "C-x 5 b" 'consult-buffer-other-frame
 "C-x r b" 'consult-bookmark
 "M-y" 'consult-yank-pop
 "<help> a" 'consult-apropos
 "M-g e" 'consult-compile-error
 "M-g g" 'consult-goto-line
 "M-g M-g" 'consult-goto-line
 "M-g o" 'consult-outline
 "M-g m" 'consult-mark
 "M-g k" 'consult-global-mark
 "M-s d" 'consult-find
 "M-s D" 'consult-locate
 "M-s g" 'consult-grep
 "M-s G" 'consult-git-grep
 "M-s r" 'consult-ripgrep
 "M-s l" 'consult-line
 "M-s L" 'consult-line-multi
 "M-s m" 'consult-multi-occur
 "M-s k" 'consult-keep-lines
 "M-s u" 'consult-focus-lines
 "M-g f" 'consult-flycheck)

;; Org mode keybindings
(general-define-key
 "C-c a" 'org-agenda
 "C-c c" 'org-capture
 "C-c l" 'org-store-link
 "C-c C-r" 'org-refile)

;; macOS-specific keybindings
(when (eq system-type 'darwin)
  ;; Disable ns-popup-font-panel
  (global-unset-key (kbd "s-t"))
  
  ;; Disable horizontal scrolling with mouse wheel
  (global-set-key (kbd "<wheel-right>") 'ignore)
  (global-set-key (kbd "<wheel-left>") 'ignore)
  (global-set-key (kbd "<double-wheel-right>") 'ignore)
  (global-set-key (kbd "<double-wheel-left>") 'ignore)
  (global-set-key (kbd "<triple-wheel-right>") 'ignore)
  (global-set-key (kbd "<triple-wheel-left>") 'ignore))

;; Global unset keys
(global-unset-key (kbd "C-<wheel-down>"))
(global-unset-key (kbd "C-<wheel-up>"))

(provide 'init-keybindings)
;;; init-keybindings.el ends here
