;;; init-web.el --- web -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package web-mode
  :after smartparens
  :mode
  (("\\.html?\\'" . web-mode)
   ("\\.phtml?\\'" . web-mode)
   ("\\.tpl\\.php\\'" . web-mode)
   ("\\.[agj]sp\\'" . web-mode)
   ("\\.as[cp]x\\'" . web-mode)
   ("\\.erb\\'" . web-mode)
   ("\\.mustache\\'" . web-mode)
   ("\\.djhtml\\'" . web-mode))
  :custom
  (web-mode-markup-indent-offset 2)
	(web-mode-css-indent-offset 2)
	(web-mode-code-indent-offset 2)
  (web-mode-script-padding 2)
  (web-mode-style-padding 2)
	(web-mode-enable-auto-quoting t)
	(web-mode-enable-auto-pairing nil)
  (web-mode-enable-auto-closing t) ;; Autocomplete </ instantly.
  (web-mode-auto-close-style 2) ;; Insert matching tags automatically.
  ;; Disable `web-mode' automatically re-indenting a bunch of
  ;; surrounding code when you paste anything.
  (web-mode-enable-auto-indentation nil)
  :config
  (sp-with-modes '(web-mode)
    (sp-local-pair "%" "%"
                   :unless '(sp-in-string-p)
                   :post-handlers '(((lambda (&rest _ignored)
                                       (just-one-space)
                                       (save-excursion (insert " ")))
                                     "SPC" "=" "#")))
    (sp-local-tag "%" "<% "  " %>")
    (sp-local-tag "=" "<%= " " %>")
    (sp-local-tag "#" "<%# " " %>")))

(provide 'init-web)
;;; init-web.el ends here
