;;TODO: experiment with `sp-restrict-to-object-interactive' with `sp-prefix-pair-object'
;;to only navigate using parens and not also symbols. Test in a C-like language
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

  ;;TODO: copy the way cleverparens does it?
  ;;https://github.com/emacs-evil/evil-cleverparens?tab=readme-ov-file#slurping-and-barfing
  (eriks/defkey-repeat sp-slurp-barf
    :states 'normal
    :prefix eriks/leader
    "]" 'sp-forward-slurp-sexp
    "[" 'sp-backward-slurp-sexp
    "}" 'sp-forward-barf-sexp
    "{" 'sp-backward-barf-sexp)

  (eriks/defkey-repeat sp-transpose
    :states 'normal
    :prefix eriks/leader
    "C-t" 'sp-transpose-sexp)

  (eriks/leader-def 'normal
    ;;TODO: ta tillbaks kill surrounding, för den fixade whitespace efteråt, jämfört med
    ;;ds). Kanske ska få den att fixa whitespace istället?
    "C-s" 'sp-split-sexp
    "C-j" 'sp-join-sexp
    "C-d" 'sp-splice-sexp-killing-around)

  :ghook ('prog-mode-hook '(show-smartparens-mode smartparens-mode))
  :custom
  (sp-navigate-interactive-always-progress-point t)
  (sp-autodelete-closing-pair nil)
  (sp-autodelete-opening-pair nil)
  (sp-autodelete-pair nil)
  (sp-echo-match-when-invisible nil)
  (sp-escape-quotes-after-insert nil)
  (sp-highlight-pair-overlay nil)
  (sp-navigate-reindent-after-up nil)
  (sp-navigate-reindent-after-up-in-string nil)
  :general-config
  ('motion
   ;;TODO: should these be in a repeat map?
   :prefix "g"
   "]" 'sp-next-sexp
   "[" 'sp-previous-sexp
   ">" 'sp-down-sexp
   "<" 'sp-backward-down-sexp)
  ('motion
   "]" 'sp-forward-sexp
   "[" 'sp-backward-sexp
   "<" 'sp-backward-up-sexp
   ">" 'sp-up-sexp
   "(" 'sp-beginning-of-sexp
   ")" 'sp-end-of-sexp)
  ('insert
   "C-u" 'sp-up-sexp))
