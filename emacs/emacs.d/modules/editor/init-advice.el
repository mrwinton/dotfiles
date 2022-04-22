;;; init-advice.el --- advice customisations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Package `which-key' displays the key bindings and associated
;; commands following the currently-entered key prefix in a popup.
(use-package which-key
  :defer 1
  :custom
  (which-key-idle-delay 1)
  :config
  (which-key-mode))

(use-package helpful
  :commands (helpful-callable helpful-key helpful-variable helpful-command helpful-at-point)
  :bind (("C-h f"   . helpful-callable)
         ("C-h k"   . helpful-key)
         ("C-h v"   . helpful-variable)
         ("C-h x"   . helpful-command)
         ("C-h C-h" . helpful-at-point)))

(use-package devdocs
  :bind (:map prog-mode-map
              ("C-c . d" . mrwinton/devdocs-lookup+))
  :init
  (defvar devdocs-major-mode-docs-alist
    '((ruby-mode . ("ruby~3" "rails~7"))
      (enh-ruby-mode . ("ruby~3" "rails~7"))
      (css-mode . ("css"))
      (html-mode . ("html"))
      (js-mode . ("javascript"))
      (js2-mode . ("javascript"))
      (emacs-lisp-mode . ("elisp")))
    "Alist of MAJOR-MODE and list of docset names.")

  (mapc
   (lambda (e)
     (add-hook (intern (format "%s-hook" (car e)))
               (lambda ()
                 (setq-local devdocs-current-docs (cdr e)))))
   devdocs-major-mode-docs-alist)

  (defun mrwinton/devdocs-lookup+()
    "Look up a DevDocs documentation entry.
Install the doc if it's not installed."
    (interactive)

    ;; Install the doc if it's not installed
    (mapc
     (lambda (str)
       (let* ((docs (split-string str " "))
              (doc (if (length= docs 1)
                       (downcase (car docs))
                     (concat (downcase (car docs)) "~" (downcase (cdr docs))))))
         (unless (file-exists-p
                  (expand-file-name "metadata" (expand-file-name doc devdocs-data-dir)))
           (message "Installing %s" str)
           (devdocs-install doc))))
     (alist-get major-mode devdocs-major-mode-docs-alist))

    ;; Lookup the symbol at point
    (if-let ((symbol (symbol-at-point)))
        (devdocs-lookup nil (symbol-name symbol))
      (message "No symbol to lookup!"))))

(provide 'init-advice)
;;; init-advice.el ends here
