;;; init-keybindings.el --- keybindings configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package which-key
  :defer 1
  :init
  (which-key-mode)
  (which-key-setup-minibuffer)
  :config
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
   "." '(counsel-find-file :which-key "find file")
   "," '(counsel-recentf :which-key "recent files")
   "TAB" '(switch-to-prev-buffer :which-key "previous buffer")
   "SPC" '(counsel-find-file :which-key "M-x")
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
   "bb" '(counsel-switch-buffer :which-key "switch buffers")
   "bd" '(kill-buffer :which-key "delete buffer")
   "bs" '(mrw/switch-to-scratch-buffer :which-key "scratch buffer")
   "bm" '(mrw/kill-other-buffers :which-key "kill other buffers")
   "bi" '(clone-indirect-buffer  :which-key "indirect buffer")
   "br" '(revert-buffer :which-key "revert buffer")
   "by" '(mrw/yank-buffer-path :which-key "yank buffer path")
   "bY" '(mrw/yank-buffer-path-relative-to-project :which-key "yank relative buffer path")

   ;; Files
   "f" '(nil :which-key "files")
   "fb" '(counsel-bookmark :which-key "bookmarks")
   "ff" '(counsel-find-file :which-key "find file")
   "fn" '(mrw/new-empty-buffer :which-key "new file")
   "fr" '(counsel-recentf :which-key "recent files")
   "fR" '(rename-file :which-key "rename file")
   "fs" '(save-buffer :which-key "save buffer")
   "fo" '(reveal-in-osx-finder :which-key "reveal in finder")
   "fO" '(mrw/open-buffer-file-mac :which-key "open buffer file")

   ;; Help/emacs
   "h" '(nil :which-key "help/emacs")
   "hv" '(counsel-describe-variable :which-key "des. variable")
   "hb" '(counsel-descbinds :which-key "des. bindings")
   "hM" '(describe-mode :which-key "des. mode")
   "hf" '(counsel-describe-function :which-key "des. func")
   "hF" '(counsel-describe-face :which-key "des. face")
   "hk" '(helpful-key :which-key "des. key")
   "hx" '(helpful-command :which-key "des. command")

   ;; Text
   "x" '(nil :which-key "text")
   "xr" '(vr/query-replace :which-key "replace-regexp")
   "xs" '(yas-insert-snippet :which-key "insert yasnippet")
   "xf" '(flush-lines :which-key "flush-lines")
   "xj" '(crux-top-join-line :which-key "join lines")
   "xa" '(mc/mark-next-like-this :which-key "mark all like this")
   "xn" '(mc/mark-next-like-this :which-key "mark next like this")
   "xp" '(mc/mark-previous-like-this :which-key "mark previous like this")


   ;; Toggles
   "t" '(nil :which-key "toggles")
   "tt" '(toggle-truncate-lines :which-key "truncate lines")
   "tv" '(visual-line-mode :which-key "visual line mode")
   "tn" '(display-line-numbers-mode :which-key "display line numbers")
   "ta" '(mixed-pitch-mode :which-key "variable pitch mode")
   "ty" '(counsel-load-theme :which-key "load theme")
   "tR" '(read-only-mode :which-key "read only mode")
   "tI" '(toggle-input-method :which-key "toggle input method")
   "tr" '(display-fill-column-indicator-mode :which-key "fill column indicator")

   ;; Windows
   "w" '(nil :which-key "window")
   "wm" '(mrw/toggle-maximize-buffer :which-key "maximize buffer")
   "w2" '(mrw/split-window-below-and-switch :which-key "split below")
   "w3" '(mrw/split-window-right-and-switch :which-key "split right")
   "wz" '(text-scale-adjust :which-key "text zoom")
   ))

(provide 'init-keybindings)
;;; init-keybindings.el ends here
