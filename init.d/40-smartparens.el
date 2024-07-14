(use-package smartparens
  :ensure t
  :diminish
  :config
  (require 'smartparens-config)
  (defun eriks/create--newline-and-enter-sexp (&rest _ignored)
    "Open a new brace or bracket expression, with relevant newlines and indentation."
    (newline)
    (indent-according-to-mode)
    (forward-line -1)
    (indent-according-to-mode))
  (defun eriks/sp-open-on (paren modes)
    "Makes all delimeter openers in paren \"open\" after enter has been pressed."
    (when (stringp paren)
      (setq paren (list paren)))
    (dolist (i paren)
      (sp-local-pair modes i nil :post-handlers '((eriks/create--newline-and-enter-sexp "RET")
                                                  (eriks/create--newline-and-enter-sexp "<return>")))))
  :ghook ('prog-mode-hook '(show-smartparens-mode smartparens-mode))
  :custom
  (sp-autodelete-closing-pair nil)
  (sp-autodelete-opening-pair nil)
  (sp-autodelete-pair nil)
  (sp-echo-match-when-invisible nil)
  (sp-escape-quotes-after-insert nil)
  (sp-highlight-pair-overlay nil)
  (sp-navigate-reindent-after-up nil)
  (sp-navigate-reindent-after-up-in-string nil)
  :general
  ('normal
   :prefix eriks/leader
   "]" 'sp-forward-slurp-sexp
   "[" 'sp-backward-slurp-sexp
   "}" 'sp-forward-barf-sexp
   "{" 'sp-backward-barf-sexp)
  ('motion
   "][" 'sp-next-sexp
   "]]" 'sp-forward-sexp ;; sp-forward-parallel-sexp
   "[[" 'sp-backward-sexp
   "[]" 'sp-previous-sexp
   "[{" 'sp-backward-up-sexp
   "]}" 'sp-up-sexp
   "[d" 'sp-backward-down-sexp
   "]d" 'sp-down-sexp
   "(" 'sp-beginning-of-sexp
   ")" 'sp-end-of-sexp)
  ('insert
   "C-u" 'sp-up-sexp))
