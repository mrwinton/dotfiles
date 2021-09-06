;;; init-graphql.el --- graphql -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package graphql-mode
  :mode (("\\.gql\\'" . graphql-mode)
         ("\\.graphql'" . graphql-mode)))

(provide 'init-graphql)
;;; init-graphql.el ends here
