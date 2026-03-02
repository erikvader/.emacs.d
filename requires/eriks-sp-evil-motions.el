(evil-define-motion eriks/sp-jump-item ()
  "Like `evil-jump-item', but with smartparens definitions.

This jumps between be start of each opening or closing pair. It doesn't
work if point is anywhere else than the start, so multi-character pairs
don't work so well with this motion."
  :jump t
  :type inclusive
  (let ((use (or (sp-get-sexp)
                 (user-error "No sexp found"))))
    (sp-get use
      (goto-char (if (>= (point) :end-in)
                     :beg
                   :end-in)))))

(evil-define-motion eriks/sp-evil-end-of-hybrid-sexp ()
  "Move to end of hybrid sexp"
  :jump t
  :type exclusive
  (let ((use (or (sp-get-hybrid-sexp)
                 (user-error "No hybrid sexp found"))))
    (sp-get use
      (goto-char :end-suf))))

(evil-define-motion eriks/sp-evil-beg-of-hybrid-sexp ()
  "Move to beginning of hybrid sexp"
  :jump t
  :type exclusive
  (let ((use (or (sp-get-hybrid-sexp)
                 (user-error "No hybrid sexp found"))))
    (sp-get use
      (goto-char :beg-prf))))

(evil-define-text-object eriks/evil-sp-inner-hybrid-sexp (count &optional beg end type)
  "Text object for the hybrid sexp, according to smartparens"
  (if-let ((bounds (sp-get-hybrid-sexp)))
      (sp-get bounds
        (evil-range :beg-prf :end-suf))
    (user-error "Not inside a hybrid sexp")))

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

(provide 'eriks-sp-evil-motions)
