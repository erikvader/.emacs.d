(use-package smartparens
  :ensure t
  :config
  (defun eriks/create-newline-and-enter-sexp (&rest _ignored)
    "Open a new brace or bracket expression, with relevant newlines and indentation."
    (newline)
    (indent-according-to-mode)
    (forward-line -1)
    (indent-according-to-mode))

  (sp-local-pair
   '(c-mode java-mode css-mode js-mode rust-mode)
   "{" nil :post-handlers '((eriks/create-newline-and-enter-sexp "RET")))
  (sp-local-pair
   '(js-mode)
   "[" nil :post-handlers '((eriks/create-newline-and-enter-sexp "RET"))))
