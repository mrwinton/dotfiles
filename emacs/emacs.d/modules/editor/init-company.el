;;; init-company.el --- company -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package company
  :hook (after-init . company-mode)
  :custom
  ;; Always display the entire suggestion list onscreen, placing it
  ;; above the cursor if necessary.
  (company-tooltip-minimum company-tooltip-limit)

  ;; Always display suggestions in the tooltip, even if there is only
  ;; one. Also, don't display metadata in the echo area. (This
  ;; conflicts with ElDoc.)
  (company-frontends '(company-pseudo-tooltip-frontend))

  ;; Make completions display as soon as possible
  (company-idle-delay 0.0)
  (company-echo-delay 0.0)

  ;; Make completions display after only typed one character, instead
  ;; of three.
  (company-minimum-prefix-length 1)

  ;; Always display the entire suggestion list onscreen, placing it
  ;; above the cursor if necessary.
  (company-tooltip-minimum company-tooltip-limit)

  ;; Show quick-reference numbers in the tooltip. (Select a completion
  ;; with M-1 through M-0.)
  (company-show-numbers t)

  ;; Prevent non-matching input (which will dismiss the completions
  ;; menu), but only if the user interacts explicitly with Company.
  (company-require-match #'company-explicit-action-p)

  ;; Make the `company-dabbrev' backend fully case-sensitive, to
  ;; improve the UX when working with domain-specific words that have
  ;; particular casing.
  (company-dabbrev-ignore-case nil)
  (company-dabbrev-downcase nil)

  ;; Only search the current buffer to get suggestions for
  ;; `company-dabbrev' (a backend that creates suggestions from text
  ;; found in your buffers). This prevents Company from causing lag
  ;; once you have a lot of buffers open.
  (company-dabbrev-other-buffers nil)

  ;; When candidates in the autocompletion tooltip have additional
  ;; metadata, like a type signature, align that information to the
  ;; right-hand side. This usually makes it look neater.
  (company-tooltip-align-annotations t)

  ;; Disable company in the following modes.
  (company-global-modes '(not erc-mode message-mode help-mode
                              gud-mode eshell-mode shell-mode))
  :config
  (global-company-mode +1))

(use-package company-quickhelp
  :after company
  :config
  (company-quickhelp-mode))

;; Complete for web,html,emmet,jade,slim modes
(use-package company-web
  :after company
  :config
  (add-to-list 'company-backends 'company-web-html))

(provide 'init-company)
;;; init-company.el ends here
