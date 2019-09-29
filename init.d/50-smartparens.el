(use-package smartparens
  :ensure t
  :after evil
  :diminish
  :config
  (require 'smartparens-config)
  (defun eriks/create-newline-and-enter-sexp (&rest _ignored)
    "Open a new brace or bracket expression, with relevant newlines and indentation."
    (newline)
    (indent-according-to-mode)
    (forward-line -1)
    (indent-according-to-mode))
  (sp-local-pair
   '(c-mode java-mode css-mode js-mode rust-mode ess-r-mode c++-mode)
   "{" nil :post-handlers '((eriks/create-newline-and-enter-sexp "RET")))
  (sp-local-pair
   '(js-mode)
   "[" nil :post-handlers '((eriks/create-newline-and-enter-sexp "RET")))
  (sp-local-pair 'm4-mode "`" "'" :actions '(insert autoskip navigate))
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
   :infix "p"
   "]" 'sp-forward-slurp-sexp
   "[" 'sp-backward-slurp-sexp
   "}" 'sp-forward-barf-sexp
   "{" 'sp-backward-barf-sexp
   "s" 'sp-split-sexp
   "j" 'sp-join-sexp
   "b" 'sp-extract-before-sexp
   "a" 'sp-extract-after-sexp
   "u" 'sp-splice-sexp
   "k" 'sp-kill-sexp
   "d" 'sp-splice-sexp-killing-around)
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

(use-package evil-cleverparens
  :ensure t
  :after (:and smartparens evil)
  :general
  ('inner
   "d" 'evil-cp-inner-defun)
  ('outer
   "d" 'evil-cp-a-defun))
