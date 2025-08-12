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

(provide 'init-keybindings)
;;; init-keybindings.el ends here
