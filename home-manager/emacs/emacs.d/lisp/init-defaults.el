;;; init-defaults.el --- default configuration  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Configure Emacs defaults, stolen from and inspired by:
;; - https://github.com/hlissner/doom-emacs/blob/develop/core/core.el
;; - https://git.sr.ht/~technomancy/better-defaults/tree/master/item/better-defaults.el

;;; Code:

;; A second, case-insensitive pass over `auto-mode-alist' is time wasted, and
;; indicates misconfiguration (don't rely on case insensitivity for file names).
(setq auto-mode-case-fold nil)

;; Disable bidirectional text rendering for a modest performance boost. I've set
;; this to `nil' in the past, but the `bidi-display-reordering's docs say that
;; is an undefined state and suggest this to be just as good:
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

;; Disabling the BPA makes redisplay faster, but might produce incorrect display
;; reordering of bidirectional text with embedded parentheses and other bracket
;; characters whose 'paired-bracket' Unicode property is non-nil.
(when (version< emacs-version "28")
  (setq bidi-inhibit-bpa t))  ; Emacs 27 only

;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions
;; in non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)


;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)







;; Remove command line options that aren't relevant to macOS.
(setq command-line-ns-option-alist nil)

;; Emacs is essentially one huge security vulnerability, what with all the
;; dependencies it pulls in from all corners of the globe. Let's try to be at
;; least a little more discerning.
(setq gnutls-verify-error (not (getenv-internal "INSECURE"))
      gnutls-algorithm-priority
      (when (boundp 'libgnutls-version)
        (concat "SECURE128:+SECURE192:-VERS-ALL"
                (if (and (not (version< emacs-version "26.3"))
                         (>= libgnutls-version 30605))
                    ":+VERS-TLS1.3")
                ":+VERS-TLS1.2"))
      ;; `gnutls-min-prime-bits' is set based on recommendations from
      ;; https://www.keylength.com/en/4/
      gnutls-min-prime-bits 3072
      tls-checktrust gnutls-verify-error
      ;; Emacs is built with `gnutls' by default, so `tls-program' would not be
      ;; used in that case. Otherwise, people have reasons to not go with
      ;; `gnutls', we use `openssl' instead. For more details, see
      ;; https://redd.it/8sykl1
      tls-program '("openssl s_client -connect %h:%p -CAfile %t -nbio -no_ssl3 -no_tls1 -no_tls1_1 -ign_eof"
                    "gnutls-cli -p %p --dh-bits=3072 --ocsp --x509cafile=%t \
--strict-tofu --priority='SECURE192:+SECURE128:-VERS-ALL:+VERS-TLS1.2:+VERS-TLS1.3' %h"
                    ;; compatibility fallbacks
                    "gnutls-cli -p %p %h"))

;; Disable warnings from legacy advice system. They aren't useful, and what can
;; we do about them, besides changing packages upstream?
(setq ad-redefinition-action 'accept)

;; Get rid of "For information about GNU Emacs..." message at startup, unless
;; we're in a daemon session where it'll say "Starting Emacs daemon." instead,
;; which isn't so bad.
(unless (daemonp)
  (advice-add #'display-startup-echo-area-message :override #'ignore))

;; Reduce *Message* noise at startup. An empty scratch buffer (or the dashboard)
;; is more than enough.
(setq inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

;; Override Emacs’ default mechanism for making buffer names unique (using
;; suffixes like <2>, <3> etc.) with a more sensible behaviour which use parts
;; of the file names to make the buffer names distinguishable.
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; When you visit a file, point goes to the last place where it was when you
;; previously visited the same file.
(add-hook 'after-init-hook
          (lambda ()
            (save-place-mode 1)))

;; Save the session's mini-buffer history.
(setq-default history-length 1000)
(add-hook 'after-init-hook
          (lambda ()
            (savehist-mode 1)))

;; Show column numbers in the modeline.
(add-hook 'after-init-hook
          (lambda ()
            (column-number-mode 1)))

;; See matching pairs of parentheses and other characters.
(setq show-paren-delay 0.0)
(add-hook 'after-init-hook
          (lambda ()
            (show-paren-mode 1)))

;; Typed text replaces the selection if the selection is active. Otherwise,
;; typed text is just inserted at point regardless of any selection.
(delete-selection-mode t)

;; Disable Emacs' default tabs behaviour.
(setq-default indent-tabs-mode nil)

;; Use two spaces when TAB-ing.
(setq-default tab-width 2)

;; When an Emacs kill command puts text in the clipboard, the existing clipboard
;; contents are normally lost. By enabling save-interprogram-paste-before-kill
;; to t, Emacs will first save the clipboard to its kill ring, preventing you
;; from losing the old clipboard data—at the risk of high memory consumption if
;; that data turns out to be large.
(setq save-interprogram-paste-before-kill t)

;; Include '\n' when point starts at the beginning-of-line.
(setq kill-whole-line t)

;; Show all variables, functions, etc when invoking apropos.
(setq apropos-do-all t)

;; Enable middle-clicking to paste.
(setq mouse-yank-at-point t)

;; When saving or writing a file silently puts a newline at the end if there
;; isn’t already one there.
(setq require-final-newline t)

;; Avoid loading old byte-compiled files.
(setq load-prefer-newer t)


;; Start Emacs at the user root directory.
(setq default-directory "~/")

;; Automatically visit symlinked files instead of warning.
(setq vc-follow-symlinks t)

;; Fill paragraphs with only a single space.
(setq sentence-end-double-space nil)

;; Ask for confirmation before exiting Emacs.
(setq confirm-kill-emacs 'y-or-n-p)

;; Don't ask `yes/no?', ask `y/n?'
(setq use-short-answers t)

;; Always turn on syntax highlighting where possible
(add-hook 'after-init-hook
          (lambda ()
            (global-font-lock-mode t)))

;; Refresh buffers automatically when the file/buffer changes
(setq global-auto-revert-non-file-buffers t)
(add-hook 'after-init-hook
          (lambda ()
            (global-auto-revert-mode t)))

;; Disable major-mode and expensive minor modes in minified files to prevent
;; hanging
(add-hook 'after-init-hook
          (lambda ()
            (global-so-long-mode t)))

;; Treat CamelCaseSubWords as separate words in programming modes
(add-hook 'prog-mode-hook 'subword-mode)

;; When saving a file that starts with `#!', make it executable
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; When saving a file in a directory that doesn't exist, offer
;; to (recursively) create the file's parent directories.
(add-hook 'before-save-hook
          (lambda ()
            (when buffer-file-name
              (let ((dir (file-name-directory buffer-file-name)))
                (when (and (not (file-exists-p dir))
                           (y-or-n-p (format "Directory %s does not exist. Create it?" dir)))
                  (make-directory dir t))))))

;; Do not show system dialog boxes.
(setq use-file-dialog nil)
(setq use-dialog-box nil)

;; Cull duplicates in the kill ring to reduce bloat and make the kill ring.
(setq kill-do-not-save-duplicates t)

;; An archaic default in the age of widescreen 4k displays? I disagree. We still
;; frequently split our terminals and editor frames, or have them side-by-side,
;; using up more of that newly available horizontal real-estate.
(setq-default fill-column 80)

;; Disable the warning "X and Y are the same file". It's fine to ignore this
;; warning as it will redirect you to the existing buffer anyway.
(setq find-file-suppress-same-file-warnings t)

;; Follow symlinks when opening files. This has the concrete impact,
;; for instance, that when you edit init.el with M-P e e i and then
;; later do C-x C-f, you will be in the Radian repository instead of
;; your home directory.
(setq find-file-visit-truename t)

;; Default to soft line-wrapping in text modes. It is more sensibile for text
;; modes, even if hard wrapping is more performant.
(add-hook 'text-mode-hook #'visual-line-mode)

;; Allow doing a command that requires candidate-selection when you
;; are already in the middle of candidate-selection. Sometimes it's
;; handy!
(setq enable-recursive-minibuffers t)

;; Don't make backup files.
(setq make-backup-files nil)

;; Don't make autosave files.
(setq auto-save-default nil)

;; Don't make lockfiles.
(setq create-lockfiles nil)

;; Trigger auto-fill after punctutation characters, not just white-space.
(mapc
 (lambda (c)
   (set-char-table-range auto-fill-chars c t))
 "!-=+]};:'\",.?")

;; Prefer UTF-8
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))



(provide 'init-defaults)
;;; init-defaults.el ends here
