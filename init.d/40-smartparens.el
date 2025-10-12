;;TODO: experiment with `sp-restrict-to-object-interactive' with `sp-prefix-pair-object'
;;to only navigate using parens and not also symbols. Test in a C-like language
(use-package smartparens
  :ensure t
  :diminish
  :config
  (add-to-list 'sp--html-modes 'mhtml-mode)
  (require 'smartparens-config)

  (defun eriks/create--newline-and-enter-sexp (&rest _ignored)
    "Open a new brace or bracket expression, with relevant newlines and indentation."
    (save-excursion
      (insert "\n")
      (indent-according-to-mode))
    (indent-according-to-mode))

  (defun eriks/sp-open-on (paren modes)
    "Makes all delimeter openers in paren \"open\" after enter has been pressed."
    (when (stringp paren)
      (setq paren (list paren)))
    (dolist (i paren)
      (sp-local-pair modes i nil :post-handlers '((eriks/create--newline-and-enter-sexp "RET")
                                                  (eriks/create--newline-and-enter-sexp "<return>")))))

  (evil-define-text-object eriks/evil-sp-a-sexp (count &optional beg end type)
    "Same as `eriks/evil-sp-inner-sexp', but also includes the delimiters,
prefix and suffix."
    (if-let ((bounds (sp-get-enclosing-sexp)))
        (sp-get bounds
          (evil-range :beg-prf :end-suf))
      (user-error "Not inside an sexp")))

  (evil-define-text-object eriks/evil-sp-inner-sexp (count &optional beg end type)
    "Text object for the inner parts of the enclosing delimiters, according
to smartparens"
    (if-let ((bounds (sp-get-enclosing-sexp)))
        (sp-get bounds
          (evil-range :beg-in :end-in))
      (user-error "Not inside an sexp")))

  ;;NOTE: remove normal state binding to let the motion state ones through
  (general-unbind 'normal "[" "]")

  ;;NOTE: I'm tired of seing the unmatched expression error message
  (cl-callf2 assq-delete-all :unmatched-expression sp-message-alist)

  (eriks/leader-def 'normal 'smartparens-mode-map
    :infix "s"
    ;;TODO: another binding for sp-transpose-hybrid-sexp?
    "t" 'sp-transpose-sexp
    "w" 'sp-swap-enclosing-sexp
    "s" 'sp-split-sexp
    "j" 'sp-join-sexp
    "d" 'sp-splice-sexp-killing-around)

  :ghook ('prog-mode-hook '(show-smartparens-mode smartparens-mode))
  :custom
  (sp-use-subword t)
  (sp-navigate-interactive-always-progress-point t)
  (sp-autodelete-closing-pair t)
  (sp-autodelete-opening-pair t)
  (sp-autodelete-pair t)
  (sp-echo-match-when-invisible nil)
  (sp-escape-quotes-after-insert t)
  (sp-highlight-pair-overlay nil)
  (sp-navigate-reindent-after-up nil)
  (sp-navigate-reindent-after-up-in-string nil)
  :general-config
  ('normal
   'smartparens-mode-map
   :prefix ">"
   ;;TODO: add hybrid functions
   ;; "}" 'sp-slurp-hybrid-sexp
   ")" 'sp-forward-slurp-sexp
   "(" 'sp-backward-barf-sexp)
  ('normal
   'smartparens-mode-map
   :prefix "<"
   ;;TODO: "}" 'sp-dedent-adjust-sexp
   "(" 'sp-backward-slurp-sexp
   ")" 'sp-forward-barf-sexp)
  ('motion
   'smartparens-mode-map
   :prefix "]"
   "[" 'sp-next-sexp
   "]" 'sp-forward-sexp
   ")" 'sp-end-of-next-sexp
   "(" 'sp-beginning-of-next-sexp
   "j" 'sp-down-sexp
   "k" 'sp-up-sexp)
  ('motion
   'smartparens-mode-map
   :prefix "["
   "]" 'sp-previous-sexp
   "[" 'sp-backward-sexp
   "(" 'sp-beginning-of-previous-sexp
   ")" 'sp-end-of-previous-sexp
   "j" 'sp-backward-down-sexp
   "k" 'sp-backward-up-sexp)
  ;; NOTE: it doesn't work to bind to 'inner and 'outer with a mode
  ('(operator visual)
   'smartparens-mode-map
   "is" 'eriks/evil-sp-inner-sexp
   "as" 'eriks/evil-sp-a-sexp)
  ('motion
   'smartparens-mode-map
   "(" 'sp-beginning-of-sexp
   ")" 'sp-end-of-sexp))

(use-package eriks-sp-surround
  :general-config
  ('operator
   'smartparens-mode-map
   "s" 'eriks/sp-surround-operator)
  ('visual
   'smartparens-mode-map
   "s" 'eriks/sp-surround))
